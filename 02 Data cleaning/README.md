# Data cleaning

Here is explained how the data from the Faculty of Science pages were cleaned.

We found 2 main problems while cleaning the data base:

- **Missing data**

We found web pages without any information, or classes without classroom or number of students, or teacher's name. For example:

http://www.fciencias.unam.mx/docencia/horarios/20081/1556/803

http://www.fciencias.unam.mx/docencia/horarios/20081/119/4

http://www.fciencias.unam.mx/docencia/horarios/20112/119/630

http://www.fciencias.unam.mx/docencia/horarios/20091/119/841

- **Repeated data**

There are different syllabus, according to each bachelor's degree. So there are some subjects which are repeated because of its name or the syllabus' year. For example:

_Same class, different name:_

http://www.fciencias.unam.mx/docencia/horarios/20201/217/1712

http://www.fciencias.unam.mx/docencia/horarios/20201/20171739

_Same class, different  syllabus' year:_

http://www.fciencias.unam.mx/docencia/horarios/20082/1556/803

http://www.fciencias.unam.mx/docencia/horarios/20082/218/803

Besides those problems we found certain web pages that have another format so the _SelectorGadget_ app wasn't able to extract some data.

- **Other problems**

1. _Strings instead of numbers_

![fig](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_one_student.PNG)

2. _Classes at different hours_

![fig](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_class_many_hours.PNG)

3. _Extra info_

![fig](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_group_diff_structure.png)
