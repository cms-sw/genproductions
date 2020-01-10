import string 

def fillTemplatedFile(template_file_name, out_file_name, template_dict, openmode = "a"):
    with open(template_file_name, "r") as templateFile:
        source = string.Template(templateFile.read())
        result = source.substitute(template_dict)
    with open(out_file_name, openmode) as outFile:
        outFile.write(result)
