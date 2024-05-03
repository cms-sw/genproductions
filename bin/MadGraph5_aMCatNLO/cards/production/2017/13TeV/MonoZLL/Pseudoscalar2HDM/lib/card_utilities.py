import os

def find_all_matching(path,pattern):
    '''
    For all files in a subdirectory of a given path that match a pattern
    '''
    if not os.path.exists(path):
        raise RuntimeError("Invalid path '{0}'".format(path))
    result = []
    for root, dirs, files in os.walk(path):
        for thisfile in files:
            if fnmatch.fnmatch(thisfile, pattern):
                result.append(os.path.join(root, thisfile ))
    return result



### Stolen from https://stackoverflow.com/a/31499114
def sed_inplace(filename, pattern, repl):
    import re, shutil, tempfile
    '''
    Perform the pure-Python equivalent of in-place `sed` substitution: e.g.,
    `sed -i -e 's/'${pattern}'/'${repl}' "${filename}"`.
    '''
    # For efficiency, precompile the passed regular expression.
    pattern_compiled = re.compile(pattern)

    # For portability, NamedTemporaryFile() defaults to mode "w+b" (i.e., binary
    # writing with updating). This is usually a good thing. In this case,
    # however, binary writing imposes non-trivial encoding constraints trivially
    # resolved by switching to text writing. Let's do that.
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as tmp_file:
        with open(filename) as src_file:
            for line in src_file:
                tmp_file.write(pattern_compiled.sub(repl, line))

    # Overwrite the original file with the munged temporary file in a
    # manner preserving file attributes (e.g., permissions).
    shutil.copystat(filename, tmp_file.name)
    shutil.move(tmp_file.name, filename)


