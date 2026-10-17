module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- The welcome page at '/' is served by the static controller below.
-- Additional [routes|...|] blocks get appended by `new-controller`.
[routes|StaticController
GET /    WelcomeAction
|]

[routes|StudentsController
GET /Students StudentsAction
GET /NewStudent NewStudentAction
POST /CreateStudent CreateStudentAction
GET /ShowStudent?studentId ShowStudentAction
GET /EditStudent?studentId EditStudentAction
POST /UpdateStudent?studentId UpdateStudentAction
DELETE /DeleteStudent?studentId DeleteStudentAction
|]


[routes|CoursesController
GET /Courses CoursesAction
GET /NewCourse NewCourseAction
POST /CreateCourse CreateCourseAction
GET /ShowCourse?courseId ShowCourseAction
GET /EditCourse?courseId EditCourseAction
POST /UpdateCourse?courseId UpdateCourseAction
DELETE /DeleteCourse?courseId DeleteCourseAction
|]

[routes|EnrollmentsController
GET /Enrollments EnrollmentsAction
GET /NewEnrollment NewEnrollmentAction
POST /CreateEnrollment CreateEnrollmentAction
GET /EditEnrollment?enrollmentId EditEnrollmentAction
POST /UpdateEnrollment?enrollmentId UpdateEnrollmentAction
DELETE /DeleteEnrollment?enrollmentId DeleteEnrollmentAction
|]

[routes|GradesController
GET /Grades GradesAction
GET /NewGrade NewGradeAction
POST /CreateGrade CreateGradeAction
GET /EditGrade?gradeId EditGradeAction
POST /UpdateGrade?gradeId UpdateGradeAction
DELETE /DeleteGrade?gradeId DeleteGradeAction
|]

[routes|ReportsController
GET /Reports ReportsAction
|]

[routes|UsersController
GET /Users UsersAction
GET /NewUser NewUserAction
POST /CreateUser CreateUserAction
GET /EditUser?userId EditUserAction
POST /UpdateUser?userId UpdateUserAction
DELETE /DeleteUser?userId DeleteUserAction
|]

[routes|SessionsController
GET /NewSession NewSessionAction
POST /CreateSession CreateSessionAction
DELETE /DeleteSession DeleteSessionAction
|]
