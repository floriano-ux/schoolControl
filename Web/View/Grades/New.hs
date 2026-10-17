module Web.View.Grades.New where
import Web.View.Prelude
import Web.View.Grades.Form (renderForm)

data NewView = NewView
    { grade  :: Grade
    , opcoes :: [OpcaoMatricula]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Lançar notas</h1>
        {renderForm grade opcoes "Lançar notas"}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Notas" GradesAction
                , breadcrumbText "Lançar notas"
                ]
