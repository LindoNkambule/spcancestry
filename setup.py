from setuptools import setup, find_packages

with open('README.md', 'r') as fh:
      long_description = fh.read()

classifiers = [
      'Development Status :: 4 - Beta',
      'Environment :: Console',
      'Intended Audience :: Science/Research',
      'License :: OSI Approved :: MIT License',
      'Programming Language :: Python :: 3',
      'Programming Language :: Python :: 3.2',
      'Programming Language :: Python :: 3.3',
      'Programming Language :: Python :: 3.4',
      'Programming Language :: Python :: 3.5',
      'Programming Language :: Python :: 3.6',
      'Programming Language :: Python :: 3.7',
      'Programming Language :: Python :: 3.8',
      'Topic :: Scientific/Engineering :: Bio-Informatics',
      'Operating System :: POSIX',
      'Operating System :: Unix',
      'Operating System :: MacOS'
]

setup(name='spcancestry',
      version='0.1.0',
      author='Lindokuhle Nkambule',
      author_email='lnkambule@hsph.harvard.edu',
      url='https://github.com/LindoNkambule/spcancestry',
      project_urls={"GitHub": "https://github.com/LindoNkambule/spcancestry"},
      description='SPCAncestry: A Python package for inferring population ancestry.',
      long_description=long_description,
      long_description_content_type="text/markdown",
      license='MIT',
      packages=find_packages(),
      classifiers=classifiers,
      keywords='',
      install_requires=['hail', 'pandas', 'sklearn'],
      zip_safe=False
      )
