class ExtraBuildRule:

    def __init__(self, template):
        self.template = template

    def isPublic(self, klass):
        return klass == "LIBRARY"

    def Project(self):
        common = self.template
        common.addProductDirMap("bin", '/tests$', "SCRAMSTORENAME_TESTS_BIN", 1)
        common.addProductDirMap("lib", '/tests$', "SCRAMSTORENAME_TESTS_LIB", 1)
        common.addSymLinks("src/LCG include/LCG 1 .")
        return True

    def Extra_template(self):
        common = self.template
        common.pushstash()
        common.dict_template()
        common.popstash()
        return True
