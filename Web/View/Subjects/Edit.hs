module Web.View.Subjects.Edit where
import Web.View.Prelude
import Web.View.Subjects.Form (renderForm)

data EditView = EditView
    { subject :: Subject
    , courses     :: [Course]
    , professores :: [User]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar disciplina</h1>
        {renderForm subject courses professores "Salvar alterações"}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Disciplinas" SubjectsAction
                , breadcrumbText "Editar disciplina"
                ]
