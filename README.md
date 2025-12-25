# WRF-Chem-2DVBS-Generator
This collection of scripts processes 2D-VBS-related configurations and generates source code files compatible with WRF-Chem compilation.

Below are a description of the items in this collection:

1. The "data" folder contains configurations of the 2D-VBS framework (e.g., grid size and parameters) and relevant input data.
2. The "gen" folder contains the generated source code files (i.e., outputs).
3. The "cache" folder contains the intermediate files produced during source generation; and they are constantly overwritten.
4. The "functions" folder contains some utility functions that are needed; no modification required herein.

5. The "*.py" scripts are the splitted steps for source generation. They can be manually executed, OR
6. The "run.sh" organizes the "*.py" files and executes them in an automated manner. So, you would simple run "./run.sh" for source generation.
