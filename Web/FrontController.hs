module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Courses
import Web.Controller.Enrollments
import Web.Controller.Grades
import Web.Controller.Reports
import Web.Controller.Sessions
import Web.Controller.Students
import Web.Controller.Static
import Web.Controller.Users

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @SessionsController
        , parseRoute @UsersController
        , parseRoute @ReportsController
        , parseRoute @GradesController
        , parseRoute @EnrollmentsController
        , parseRoute @CoursesController
        , parseRoute @StudentsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
