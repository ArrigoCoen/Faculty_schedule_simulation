# Faculty_schedule_simulation
 In this repository we will simulate the schedule of Faculty of Sciences of UNAM

We will follow the next pipeline:

![fig](Figures/Fig_pipeline.png)

**(1) Data extraction**

1. We use the _posibles_url_ function, to generate all the possible URL's of the Faculty of Sciences, UNAM.

2. With the _Actualiza_list_url_ function, we erase the URL's of the web pages with no information.

3. With the _gen_m_grande_SIN_MOD_ function, we download the web pages information. To extract the information of one web page we use the _extrae_info_1_pag_ function.

**(2) Data cleaning**

The _gen_m_grande_ function, generates a matrix with the data of the web pages and cleans it. A matrix for each semester.

The _gen_m_grande_total_ function, creates a matrix with the data of several semesters. It concatenates the matrices of the _gen_m_grande_ function.

<!---
1. The _gen_m_grande_ function, cleans the data and generates a matrix with 37 columns (Subject, Teacher, Hour, hour_num, Places, Students, Classroom, Group, Bac_Degree, Syllabus, Semester, Changes, Shift, Subject_Sem, URL, Act2000, Act2006, Act2015, CdC1994, CdC2013, Mat1983, MAp2017, NomMat_Act2000, NomMat_Act2006, NomMat_Act2015, NomMat_CdC1994, NomMat_CdC2013, NomMat_Mat1983, NomMat_MAp2017, URL_Act2000, URL_Act2006, URL_Act2015, URL_CdC1994, URL_CdC2013, URL_Mat1983, URL_MAp2017, Num_materia)
-->

**(3) Students simulation**

1. We make a new matrix with the number of students per hour for each semester. Using the _gen_mat_alumnos_corregidos_ function.

2. We simulate the number of students for the next semester with the _gen_mat_demanda_alumnos_ function.

**(4a) Teachers' requests simulation _(hidden)_**

The simulation of the teachers' requests is made with the _gen_solicitudes_ function.

**(5) Skeletons' simulation**

With the matrices simulated in **(3)**, we simulate _n_ skeletons. We apply the Gaussian Mixture Model to that data and we obtain a new skeleton with the parameters of the model.

**(4b) Teachers' requests simulation _(pseudo-real)_**

The simulation of the teachers' real requests is made with the _gen_solicitudes_real_ function.

**(6) Teachers' assignment**

The _gen_asignacion_ function is used to make an assignment of subjects, teachers and hours.

**(7) Grading teachers' assignment**

The grading of each assignment is made with the _califica_asignacion_ function.

**(8) Genetic Algorithm applied to assignments**

We use the theory of Genetic Algorithm to find a good assignment. The function that we use is _AG_asignaciones_con_xlsx_. In this function we have the option to read a _.xlsx_ document with previous subjects assigned to a teacher in an hour.

**(9) Final results**

The final result is a matrix with 3 columns: Subject, Teacher and Hour. The number of rows depends of the simulation made with the Genetic Algorithm. There are some examples in https://github.com/ArrigoCoen/Faculty_schedule_simulation/tree/master/09%20Final%20results/Matrices.
