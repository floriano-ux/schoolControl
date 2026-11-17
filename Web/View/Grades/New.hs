module Web.View.Grades.New where
import Web.View.Prelude
import Web.View.Grades.Form (renderForm)

data NewView = NewView
    { grade       :: Grade
    , disciplinas :: [(Subject, Course)]
    , escolhida   :: Maybe (Subject, Course)
    , opcoes      :: [OpcaoMatricula]
    }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>Lançar notas</h1>
        {if null disciplinas then semDisciplinas else etapas}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Notas" GradesAction
                , breadcrumbText "Lançar notas"
                ]

            semDisciplinas = [hsx|
                <div class="alert alert-info">
                    Você ainda não tem disciplinas atribuídas. Peça ao administrador para vinculá-lo a uma disciplina.
                </div>
            |]

            etapas = [hsx|
                <div class="card mb-4">
                    <div class="card-body">
                        <h2 class="h6">1. Disciplina</h2>
                        <form method="GET" action={NewGradeAction} class="row g-2 align-items-end">
                            <div class="col-md-9">
                                <select name="subjectId" class="form-select" aria-label="Disciplina"
                                        onchange="this.form.requestSubmit ? this.form.requestSubmit() : this.form.submit()">
                                    {opcaoVazia}
                                    {forEach disciplinas opcaoDisciplina}
                                </select>
                            </div>
                            <div class="col-md-3">
                                <button type="submit" class="btn btn-outline-primary w-100">Carregar alunos</button>
                            </div>
                        </form>
                    </div>
                </div>
                {segundaEtapa}
            |]

            opcaoVazia = case escolhida of
                Nothing -> [hsx|<option value="" selected="selected" disabled="disabled">Selecione a disciplina</option>|]
                Just _  -> [hsx|<option value="" disabled="disabled">Selecione a disciplina</option>|]

            opcaoDisciplina par@(subject, _)
                | fmap (\(s, _) -> s.id) escolhida == Just subject.id =
                    [hsx|<option value={inputValue subject.id} selected="selected">{rotuloDaDisciplina par}</option>|]
                | otherwise =
                    [hsx|<option value={inputValue subject.id}>{rotuloDaDisciplina par}</option>|]

            segundaEtapa = case escolhida of
                Nothing -> [hsx|<p class="text-muted">Escolha a disciplina para ver os alunos matriculados nela.</p>|]
                Just par@(subject, _)
                    | null opcoes -> [hsx|
                        <div class="alert alert-info">
                            Nenhum aluno pendente em {rotuloDaDisciplina par}: não há matrículas ou todas já têm notas lançadas.
                        </div>
                    |]
                    | otherwise -> [hsx|
                        <div class="card">
                            <div class="card-body">
                                <h2 class="h6">2. Aluno e notas — {rotuloDaDisciplina par}</h2>
                                {renderForm grade subject.id opcoes "Lançar notas"}
                            </div>
                        </div>
                    |]
