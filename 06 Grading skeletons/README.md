# Grading skeletons

Here is explained how we grade the skeletons.

Before we can grade a skeleton we need the numner of students simulated and the following matrixes:

i) Number of students expected for the next semester _mat\_students\_sim_ (by hour and subject).

ii) Simulated skeleton _mat\_skeleton_

iii) Number of subjects assigned to a full-time teacher

iv) Number of students left after simulating the skeleton

v) Full-time teachers' requests

vi) Part-time teachers' requests

We have 5 different penalties:

1. **Subjects needed but not given:** The grade is reduce in one for each subject that has students in _mat\_students\_sim_ but doesn't have a group in _mat\_skeleton_.

2. **Students without class**

3. **Students simulated over the needs**

4. **Subjects requested but not given**

5. **Classes without a teacher**









