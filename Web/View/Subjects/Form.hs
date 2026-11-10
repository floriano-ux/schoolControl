module Web.View.Subjects.Form (renderForm) where
import Web.View.Prelude

renderForm :: Subject -> [Course] -> [User] -> Html -> Html
renderForm subject courses professores textoDoBotao = formFor subject [hsx|
    {(selectField #courseId courses) { fieldLabel = "Curso", placeholder = "Selecione o curso" }}
    {(textField #name) { fieldLabel = "Nome" }}
    {(numberField #workload) { fieldLabel = "Carga horária (horas)" }}
    <div class="mb-3">
        <label class="form-label" for="subject_professorId">Professor responsável</label>
        <select id="subject_professorId" name="professorId" class={classeDoSelect}>
            <option value="">Sem professor</option>
            {forEach professores opcaoProfessor}
        </select>
        {erroProfessor}
        <div class="form-text">O professor só vê alunos, matrículas e notas das disciplinas que ministra.</div>
    </div>
    {submitButton { label = textoDoBotao }}
|]
    where
        erro = getValidationFailure #professorId subject
        classeDoSelect :: Text
        classeDoSelect = if isJust erro then "form-select is-invalid" else "form-select"
        erroProfessor = case erro of
            Just mensagem -> [hsx|<div class="invalid-feedback d-block">{mensagem}</div>|]
            Nothing       -> mempty
        opcaoProfessor professor
            | subject.professorId == Just professor.id =
                [hsx|<option value={inputValue professor.id} selected="selected">{professor.name}</option>|]
            | otherwise =
                [hsx|<option value={inputValue professor.id}>{professor.name}</option>|]
