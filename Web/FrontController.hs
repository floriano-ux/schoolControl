module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Boletim
import Web.Controller.Courses
import Web.Controller.Enrollments
import Web.Controller.Grades
import Web.Controller.Reports
import Web.Controller.Registrations
import Web.Controller.Sessions
import Web.Controller.Students
import Web.Controller.Subjects
import Web.Controller.Static
import Web.Controller.Users

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @RegistrationsController
        , parseRoute @SessionsController
        , parseRoute @UsersController
        , parseRoute @ReportsController
        , parseRoute @BoletimController
        , parseRoute @GradesController
        , parseRoute @EnrollmentsController
        , parseRoute @CoursesController
        , parseRoute @StudentsController
        , parseRoute @SubjectsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
