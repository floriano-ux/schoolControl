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
    | ShowStudentAction { studentId :: !(Id User) }
    | CreateStudentAction
    | EditStudentAction { studentId :: !(Id User) }
    | UpdateStudentAction { studentId :: !(Id User) }
    | DeleteStudentAction { studentId :: !(Id User) }
    | RegenerarSenhaAction { studentId :: !(Id User) }
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

data SubjectsController
    = SubjectsAction
    | NewSubjectAction
    | ShowSubjectAction { subjectId :: !(Id Subject) }
    | CreateSubjectAction
    | EditSubjectAction { subjectId :: !(Id Subject) }
    | UpdateSubjectAction { subjectId :: !(Id Subject) }
    | DeleteSubjectAction { subjectId :: !(Id Subject) }
    deriving (Eq, Show, Data)

data EnrollmentsController
    = EnrollmentsAction
    | NewEnrollmentAction
    | ShowEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    | CreateEnrollmentAction
    | EditEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    | UpdateEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    | DeleteEnrollmentAction { enrollmentId :: !(Id Enrollment) }
    deriving (Eq, Show, Data)

data GradesController
    = GradesAction
    | NewGradeAction
    | ShowGradeAction { gradeId :: !(Id Grade) }
    | CreateGradeAction
    | EditGradeAction { gradeId :: !(Id Grade) }
    | UpdateGradeAction { gradeId :: !(Id Grade) }
    | DeleteGradeAction { gradeId :: !(Id Grade) }
    deriving (Eq, Show, Data)

data ReportsController
    = ReportsAction
    deriving (Eq, Show, Data)

data BoletimController
    = MeuBoletimAction
    deriving (Eq, Show, Data)

data UsersController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    | AtribuirDisciplinasAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

-- Cadastro público: qualquer visitante pode criar a própria conta (papel aluno).
data RegistrationsController
    = NewRegistrationAction
    | CreateRegistrationAction
    deriving (Eq, Show, Data)


-- Item do select de aluno no lançamento de notas ("Aluno (status)").
data OpcaoMatricula = OpcaoMatricula
    { matriculaId :: !(Id Enrollment)
    , rotulo      :: !Text
    } deriving (Eq, Show)


-- Login (IHP.LoginSupport): para onde mandar quem não está logado
instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User
