// Implementation of a cache for the master integrals, used in
// avholo_interface.cc.  It is assumed that Key-types are structures
// containing (arrays of) real and/or complex floating point numbers.
// Since it is supposed to be used as a cache, a new entry is always
// created when the key is not found, allowing thus to fill the
// correspond value.


#ifndef NINJA_HASH_TABLE_HH
#define NINJA_HASH_TABLE_HH

#include <vector>

namespace ninja {

  namespace detail {

    const std::size_t END_OF_HASH_PRIMES = ~static_cast<std::size_t>(0);

    // http://planetmath.org/goodhashtableprimes argues these are good
    // numbers
    const std::size_t hash_table_primes[] = {
      // these five have been added for convenience
      1,
      3,
      7,
      13,
      23,
      // the following are taken from the link above
      53,
      97,
      193,
      389,
      769,
      1543,
      3079,
      6151,
      12289,
      24593,
      49157,
      98317,
      196613,
      393241,
      786433,
      1572869,
      3145739,
      6291469,
      12582917,
      25165843,
      50331653,
      100663319,
      201326611,
      402653189,
      805306457,
      1610612741,
      END_OF_HASH_PRIMES
    };

    inline int find_prime(int min_index, std::size_t min_prime)
    {
      int i;
      for (i=min_index; detail::hash_table_primes[i] < min_prime; ++i) ;
      if (detail::hash_table_primes[i] == detail::END_OF_HASH_PRIMES) --i;
      return i;
    }

    template <typename T>
    inline std::size_t round_up(T x)
    {
      return static_cast<std::size_t>(x+T(0.5));
    }

  } // namespace detail


  // boost-like hash-combine
  inline void hash_combine(std::size_t & seed, std::size_t val)
  {
    seed ^= val + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }

  // Class for hash function using boost-like hash-combine
  template <typename T>
  struct SimpleHash {
    std::size_t operator() (const T & x) const;
  };


  namespace detail {

    template <unsigned NBytes>
    struct LastBytesCombine {

      union LastBytes {
        std::size_t i;
        char c[sizeof(std::size_t)];
        LastBytes(): i(0) {}
      };

      static void hash_combine(std::size_t & seed, const void * ptr)
      {
        LastBytes last_bytes;
        for (unsigned i=0; i < NBytes; ++i)
          last_bytes.c[i] = static_cast<const char*>(ptr)[i];
        ::ninja::hash_combine(seed,last_bytes.i);
      }
    };

    template <>
    struct LastBytesCombine<0> {
      static void hash_combine(std::size_t &, const void *) {}
    };

  } // namespace detail 


  template <typename T>
  std::size_t SimpleHash<T>::operator() (const T & x) const
  {
    std::size_t seed = sizeof(T);
    
    const std::size_t * ptr
      = static_cast<const std::size_t *>(static_cast<const void*>(& x));
    for (unsigned i=0; i < (sizeof(T) / sizeof(std::size_t)); ++i, ++ptr)
      hash_combine(seed,*ptr);
    
    detail::LastBytesCombine<(sizeof(T) % sizeof(std::size_t))>
      ::hash_combine(seed,static_cast<const void*>(ptr));
        
    return seed;
  }



  typedef float ratio_t;
  const ratio_t rehash_ratio_ = ratio_t(1);



  // HashTable class
  template <typename Key, typename Val,
            typename HashFunc = SimpleHash<Key> >
  class HashTable {
  public:

    typedef Val * ValuePtr;

    // default constructor
    HashTable()
      : buckets_(), size_(0), prime_index_(0) {}

    ~HashTable()
    {
      free();
    }

    bool empty() const
    {
      return buckets_.empty();
    }

    // enlarge the hash table to contain at least the specified
    // number of buckets
    void resize(std::size_t min_buckets);

    // keep buckets but clear the nodes
    void clear()
    {
      for (BucketsIter i = buckets_.begin(); i != buckets_.end(); ++i)
        (*i).clear();
      size_=0;
    }

    // destroy the hash table
    void free()
    {
      clear();
      buckets_.clear();
      prime_index_=0;
    }

    // Find element equal to the specified key.  If found, the second
    // parameter is set to the address of the corresping value and the
    // method returns true.  If not found, a new entry is added to the
    // hash table, the second parameter is set to the address of the
    // corresping value to be filled by the user, and the method
    // returns false.
    bool find(const Key & key, ValuePtr & value);

  private:

    bool rehashNeeded_()
    {
      using namespace detail;
      return (static_cast<ratio_t>(size_)/buckets_.size() > rehash_ratio_
              && hash_table_primes[prime_index_+1] != END_OF_HASH_PRIMES);
    }

    void rehash_();

  private:

    struct NodeList_;

    typedef typename std::vector<NodeList_>::iterator BucketsIter;

    // Class ccontaining the nodes of the hash table
    struct NodeList_ {

      // structure for a node of the hash table
      struct Node_ {
        
        Node_(const Key & ikey, std::size_t ihash_value)
          : next(), key(ikey), hash_value(ihash_value), value() {}

        Node_ * next;
        Key key;
        std::size_t hash_value;
        Val value;
      }; // struct HashTable::NodeList_::Node_

      Node_ * head;

      NodeList_(): head(0) {}

      NodeList_(const NodeList_ & other): head(other.head) {}

      Val * add(const Key & key, std::size_t hash_value)
      {
        Node_ * new_node = new Node_(key,hash_value);
        new_node->next = head;
        head = new_node;
        return & new_node->value;
      }

      void addNode(Node_ * node)
      {
        node->next = head;
        head = node;
      }

      static void moveNextNodeToList(Node_ * & prev_node, NodeList_ & list)
      {
        Node_ * node = prev_node;
        prev_node = prev_node->next;
        list.addNode(node);
      }

      bool find(const Key & key, std::size_t hash_value, ValuePtr & value)
      {
        for (Node_ * node = head; node; node = node->next)
          if (node->hash_value == hash_value && node->key == key) {
            value = & node->value;
            return true;
          }
        return false;
      }

      void clear();

    }; // struct HashTable::NodeList_

  private:

    // data members of HashTable
    std::vector<NodeList_> buckets_;
    std::size_t size_;    
    int prime_index_;

  }; // template class HashTable


  template <typename Key, typename Val, typename HashFunc>
  void HashTable<Key,Val,HashFunc>::resize(std::size_t min_buckets)
  {
    prime_index_ = detail::find_prime(prime_index_,min_buckets);
    buckets_.resize(detail::hash_table_primes[prime_index_]);
  }


  template <typename Key, typename Val, typename HashFunc>
  void HashTable<Key,Val,HashFunc>::NodeList_::clear()
  {
    Node_ * node = head;
    Node_ * next_node;

    while (node) {
      next_node = node->next;
      delete node;
      node = next_node;
    }

    head = 0;
  }


  template <typename Key, typename Val, typename HashFunc>
  bool HashTable<Key,Val,HashFunc>::find (const Key & key, ValuePtr & value)
  {
    std::size_t hash_value = HashFunc()(key);
    std::size_t hash_index = hash_value % buckets_.size();

    // Cheks if present
    if (buckets_[hash_index].find(key,hash_value,value))
      return true;

    // If it gets here, the key was not found and another entry must
    // be added
    ++size_;

    // check if rehash is needed
    if (rehashNeeded_()) {
      rehash_();
      hash_index = hash_value % buckets_.size();
    }

    // add new node and get pointer to its Value field
    value = buckets_[hash_index].add(key,hash_value);

    return false;
  }


  template <typename Key, typename Val, typename HashFunc>
  void HashTable<Key,Val,HashFunc>::rehash_()
  {
    std::size_t old_buckets_size = buckets_.size();
    resize(detail::round_up((size_+1)/rehash_ratio_));
  
    unsigned i=0;
    for (BucketsIter it = buckets_.begin(); i < old_buckets_size; ++it, ++i) {

      typename NodeList_::Node_ * node = (*it).head;
      typename NodeList_::Node_ ** node_ptr = & (*it).head;

      while (node) {
        std::size_t idx = node->hash_value % buckets_.size();
        if (idx != i) {
          NodeList_::moveNextNodeToList(*node_ptr,buckets_[idx]);
          node = *node_ptr;
        } else {
          node_ptr = & (node->next);
          node = node->next;
        }
      }

    }
  }

} // namespace ninja


#endif // NINJA_HASH_TABLE_HH
