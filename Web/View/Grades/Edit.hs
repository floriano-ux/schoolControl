module Web.View.Grades.Edit where
import Web.View.Prelude
import Web.View.Grades.Form (renderForm)

data EditView = EditView
    { grade   :: Grade
    , detalhe :: MatriculaDetalhada
    , opcoes  :: [OpcaoMatricula]
    }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Editar notas</h1>
        <p class="text-muted">Disciplina: <strong>{rotuloDaDisciplina (detalhe.disciplina, detalhe.curso)}</strong></p>
        {renderForm grade detalhe.disciplina.id opcoes "Salvar alterações"}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Notas" GradesAction
                , breadcrumbText "Editar notas"
                ]
