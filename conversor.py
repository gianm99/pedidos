"""
    Programa de conversión de imágenes BMP 24 bits a imágenes IMG.
    Convierte todas las imágenes BMP de un directorio y las guarda como IMG
    en otro directorio. Si no se indican los directorios de entrada y de
    salida, el programa realiza la conversión con los directorios por defecto.
    Instrucciones de uso:
    -h
    Muestra cómo se usa el programa.
    -i <inputdirectory>
    Para indicar el directorio en el que se encuentran las imágenes BMP.
    -o <outputdirectory>
    Para indicar el directorio en el que se guardarán las imágenes IMG.
"""


import sys
import getopt
import os
import shutil

DEFAULT_INPUT = 'bmp'
DEFAULT_OUTPUT = 'img'

def convert(inputfile,outputfile):
    """Eliminar el encabezado de un archivo .bmp y convertirlo en .img."""
    src_file=open(inputfile,'rb') # abrir fichero input (read + binary)
    src_file.read(54) # mover el cursor al final del encabezado
    dst_file=open(outputfile,'wb') # abrir fichero destino
    shutil.copyfileobj(src_file,dst_file) # copiar el contenido sin el encabezado
    print(inputfile+' guardado como '+outputfile)

def main(argv):
    inputdirectory = DEFAULT_INPUT
    outputdirectory = DEFAULT_OUTPUT
    try:
        opts, args = getopt.getopt(argv, "hi:o:")
    except getopt.GetoptError:
        print('conversor.py -i <inputdirectory> -o <outputdirectory>')
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            print('conversor.py -i <inputdirectory> -o <outputdirectory>')
            sys.exit()
        elif opt == '-i':
            inputdirectory = arg
        elif opt == '-o':
            outputdirectory = arg
    # si no existe el directorio de entrada, termina la ejecución
    if not os.path.isdir(inputdirectory):
        sys.exit()
    # crear el directorio de salida si no existe
    os.makedirs(outputdirectory, exist_ok=True)
    # recorrer todos los ficheros .bmp del directorio
    inputfile=''
    outputfile=''
    base=''
    for filename in os.listdir(inputdirectory):
        if filename.endswith('.bmp'):
            inputfile=inputdirectory+'\\'+filename
            base=filename.rpartition('.')[0] # quitar extension
            outputfile=outputdirectory+'\\'+base+'.img'
            convert(inputfile,outputfile)
    print('Todos los ficheros .bmp de ' + inputdirectory +
          ' han sido convertido a ficheros .img en ' + outputdirectory)


if __name__ == "__main__":
    main(sys.argv[1:])
