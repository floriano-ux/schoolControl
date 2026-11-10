module Web.View.Subjects.New where
import Web.View.Prelude
import Web.View.Subjects.Form (renderForm)

data NewView = NewView
    { subject :: Subject
    , courses :: [Course]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Nova disciplina</h1>
        {renderForm subject courses "Salvar"}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Disciplinas" SubjectsAction
                , breadcrumbText "Nova disciplina"
                ]
