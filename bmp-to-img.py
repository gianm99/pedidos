# Gian Lucas Martín Chamorro
"""
    Program that removes the header of all the .bmp files in a directory and
    saves them as .img files in another directory.
"""

import sys
import getopt
import os
import shutil

DEFAULT_INPUT = 'IMAGENES_CARACTERES_BMP'
DEFAULT_OUTPUT = 'IMAGENES_CARACTERES_IMG'


def convert(inputfile,outputfile):
    src_file=open(inputfile,'rb') # open inputfile (read + binary)
    src_file.read(54) # move cursor to end of header
    dst_file=open(outputfile,'wb') # open destination file
    shutil.copyfileobj(src_file,dst_file) # copy file contents without the header
    # print('File '+inputfile+' saved as '+outputfile)



def main(argv):
    inputdirectory = DEFAULT_INPUT
    outputdirectory = DEFAULT_OUTPUT
    try:
        opts, args = getopt.getopt(argv, "hi:o:")
    except getopt.GetoptError:
        print('bmp-to-img.py -i <inputdirectory> -o <outputdirectory>')
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print('bmp-to-img.py -i <inputdirectory> -o <outputdirectory>')
            sys.exit()
        elif opt == '-i':
            inputdirectory = arg
        elif opt == '-o':
            outputdirectory = arg
    # create the directories if they don't already exist
    os.makedirs(inputdirectory, exist_ok=True)
    os.makedirs(outputdirectory, exist_ok=True)
    
    # loop through all .bmp files on inputdirectory
    inputfile=''
    outputfile=''
    base=''
    for filename in os.listdir(inputdirectory):
        if filename.endswith('.bmp'):
            inputfile=inputdirectory+'\\'+filename
            base=filename.rpartition('.')[0] # remove extension
            outputfile=outputdirectory+'\\'+base
            if filename.endswith('_NB.bmp') or filename.endswith('_BN.bmp'):
                outputfile=outputfile+'_20.img'
            else:
                outputfile=outputfile+'_52.img'
            convert(inputfile,outputfile)
    
    print('All .bmp files from ' + inputdirectory +
          ' have been succesfully converted to .img files on ' + outputdirectory)


if __name__ == "__main__":
    main(sys.argv[1:])
