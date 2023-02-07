#! /usr/bin/env python

from distutils.core import setup

setup(name='ninjanumgen',
      version='1.0',
      description='Generator of Numerators for Ninja',
      author='Tiziano Peraro',
      author_email='tiziano.peraro@ed.ac.uk',
      scripts=['ninjanumgen/ninjanumgen'],
      packages = ['ninjanumgen'],
      package_data={'ninjanumgen' : ['templates/ninja_laurent.frm',
                                     'templates/ninja_opt.frm',
                                     'templates/ninjanumgen_template.cc',
                                     'templates/ninjanumgen_template.hh']}
     )
