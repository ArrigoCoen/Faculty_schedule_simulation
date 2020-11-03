# Grading skeletons

Here is explained how we grade the skeletons.

Before we can grade a skeleton we need the number of students simulated and the following matrixes:

- Number of students expected for the next semester _mat\_students\_sim_ (by hour and subject).

- Simulated skeleton _mat\_skeleton_

- Number of subjects assigned to a full-time teacher

- Number of students left after simulating the skeleton _mat\_students\_sim\_aux_

- Full-time teachers' requests

- Part-time teachers' requests

We have 5 different penalties:

1. **Subjects needed but not given:** The grade is reduce in one for each subject that has students in _mat\_students\_sim_ but doesn't have a group in _mat\_skeleton_.

2. **Students without class:** The grade is reduce in _alpha_ times the number of students left in _mat\_students\_sim\_aux_ (positive ones).

3. **Students simulated over the needs:** The grade is reduce in _-beta_ times the number of students left in _mat\_students\_sim\_aux_ (negative ones).

4. **Subjects requested but not given:** The grade is reduce in one for each subject that a full-time teacher requested but he/she wasn't assigned to it (up to 2 subjects).

5. **Classes without a teacher:** The grade is reduce in one for each teacher that can give a class that has positive number of students in _mat\_students\_sim\_aux_. 

In the next figure we can see the heatmap of the division of the matrix _mat\_students\_sim\_aux_ divided by _mat\_students\_sim_.

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_mat_demanda_aux_divided_by_mat_demanda_alumnos.png)

In the next figure we can see the graphic of the number of students simulated over the needs over the sum, by subjects, of the number of students simulated in _mat\_students\_sim_.

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_over_students_over_simulated.PNG)

If the sum, by subjects of _mat\_students\_sim_ is zero, then we assign a plus infinity to that division. _R_ doesn't graphic those data.

The range of the data is from -27 to 0 (without infinity). That means that in the worst scenario, we have simulated 27 times over the number of students needed.

The two subjects with the lowest rate are _Finanzas I_ and _Historia de las Matemáticas I_. After that we have the subjects that we can see in the following figure, those are from -15 to -1.

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_subjects_with_over_students.PNG)

In the best scenario, we didn't simulate over the number of students needed (when we reach 0).

In the next figure we can see the graphic of the number of students simulated under the needs over the sum, by subjects, of the number of students simulated in _mat\_students\_sim_.

If the sum, by subjects of _mat\_students\_sim_ is zero, then we assign a plus infinity to that division. _R_ doesn't graphic those data.

The range of the data is from 0 to 1 (without infinity). That means that in the worst scenario, when we reach 1, we didn't simulate any of the students needed. And in the best scenario, we simulated at least the number of students needed, (when we reach 0).

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_under_students_over_simulated.PNG)


In the next figure we can see the graphic of the sum of the number of students without class plus the number of students simulated over the needs.

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_sum_over_and_under_students.PNG)

In the next figure we can see the histogram of the number of students without class and the number of students simulated over the needs.

![mat_demanda_aux_divided_by_mat_demanda_alumnos](https://github.com/ArrigoCoen/Faculty_schedule_simulation/blob/master/Figures/Fig_histogram_over_and_under_students.PNG)


