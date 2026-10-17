module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data StudentsController
    = StudentsAction
    | NewStudentAction
    | ShowStudentAction { studentId :: !(Id Student) }
    | CreateStudentAction
    | EditStudentAction { studentId :: !(Id Student) }
    | UpdateStudentAction { studentId :: !(Id Student) }
    | DeleteStudentAction { studentId :: !(Id Student) }
    deriving (Eq, Show, Data)

data CoursesController
    = CoursesAction
    | NewCourseAction
    | ShowCourseAction { courseId :: !(Id Course) }
    | CreateCourseAction
    | EditCourseAction { courseId :: !(Id Course) }
    | UpdateCourseAction { courseId :: !(Id Course) }
    | DeleteCourseAction { courseId :: !(Id Course) }
    deriving (Eq, Show, Data)

data EnrollmentsController
    = EnrollmentsAction
    | NewEnrollmentAction
    | CreateEnrollmentAction
    | EditEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    | UpdateEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    | DeleteEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    deriving (Eq, Show, Data)

data GradesController
    = GradesAction
    | NewGradeAction
    | CreateGradeAction
    | EditGradeAction { gradeId :: !(Id Grade) }
    | UpdateGradeAction { gradeId :: !(Id Grade) }
    | DeleteGradeAction { gradeId :: !(Id Grade) }
    deriving (Eq, Show, Data)

data ReportsController
    = ReportsAction
    deriving (Eq, Show, Data)

data UsersController
    = UsersAction
    | NewUserAction
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)


-- Item do select de matrículas na tela de Notas ("Aluno — Curso (status)").
data OpcaoMatricula = OpcaoMatricula
    { matriculaId :: !(Id Enrollment)
    , rotulo      :: !Text
    } deriving (Eq, Show)


-- Login (IHP.LoginSupport): para onde mandar quem não está logado
instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User
