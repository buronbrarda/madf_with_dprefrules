mkdir tmp
mkdir tmp\app_lib
mkdir tmp\examples
mkdir tmp\swipl_core

xcopy app_lib tmp\app_lib
xcopy examples tmp\examples
xcopy swipl_core tmp\swipl_core

del /q tmp\app_lib\.gitignore

del /q tmp\examples\.gitignore
del /q tmp\examples\.project

del /q tmp\swipl_core\.project

xcopy app.jar tmp
xcopy readme.txt tmp

cd .\tmp

rar a -r cpref_decision_system.zip app_lib examples swipl_core app.jar readme.txt

cd ..

move /y tmp\cpref_decision_system.zip cpref_decision_system.zip

rd /s /q .\tmp

