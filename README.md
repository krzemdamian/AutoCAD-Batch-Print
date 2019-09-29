# AutoCAD-Batch-Print
This is LISP script enabling batch printing from drawing model and drawing sheets

## How to use this script:
Script contains 3 commands which are available by typing on keyboard.
1.	Command „DE” 
This is the first command user must run.  
It is used to create autocad blocks which represents paper page boundaries.
Blocks also stores information about name of the drawing.
Command should be run only once on each .dwg .dxf file because those blocks are embeded in file.
1)	Type "DE" and pres enter of space.
2)  In dialog choose "rozmiary_papieru.dat" from folder.
    In this file all available paper sizes are saved.
3)	Blocks are generating

2.	Command „DW”
This command is used to place paper boundary block on drawing.
Command must be run for each area user want to print automatically.
1)	Type „DW” and press enter or space.
2)  Choose paper size from list dialog
3)	Type drawing name (have to be unique)
4)  Type scale of drawing. This is first out of 2 inputs determining drawing scale.
5)  On drawing area click lower right corner of page boundary rectangle.
6)	Page boundary frame block should have been placed.
7)  You can manually change name of final drawing editing frame's attribute.
8)  CAUTION: "NAZWA_RYSUNKU" is the only one attribute which can be changed manually.
9)  CAUTION: You can copy frames by autocad command "COPY" but remember to change name of the drawing in frame block attribute. Because name of the page must be unique. Otherwise there will be error while printing, because script would not be able to save 2 files with the same name.

3.	Command „DR”
Command to start batch printing.
1)	Type „DR” and press enter or space.
2)  Script will ask wheter you want to print all frames or selected only.
3)  In case you choose to print selection only: place 2 diagonal vertices of rectangle which should indicate frame selection.
4)  Choose print style.
5)  Printed drawings should be saved in "DRUK" folder, which should be located inside folder where your .dwg or .dxf file is saved.
