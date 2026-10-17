module Web.View.Grades.Form (renderForm) where
import Web.View.Prelude

-- Formulário compartilhado entre "Lançar notas" e "Editar notas".
-- O texto do botão é Html (o label do submitButton é Html no IHP 1.6);
-- com OverloadedStrings basta passar uma string literal.
renderForm :: Grade -> [OpcaoMatricula] -> Html -> Html
renderForm grade opcoes textoDoBotao = formFor grade [hsx|
    {(selectField #enrollmentId opcoes) { fieldLabel = "Matrícula (aluno — curso)", placeholder = "Selecione a matrícula" }}

    <div class="row">
        <div class="col-md-6">
            <h2 class="h6 mt-2">AV1</h2>
            {nota #pe1 "Prova escrita (PE1)"}
            {nota #avf1 "Avaliação formativa (AVf1)"}
        </div>
        <div class="col-md-6">
            <h2 class="h6 mt-2">AV2</h2>
            {nota #pe2 "Prova escrita (PE2)"}
            {nota #avf2 "Avaliação formativa (AVf2)"}
        </div>
    </div>

    <h2 class="h6 mt-2">AV3 <small class="text-muted">(opcional: só se a média de AV1 e AV2 for ≥ 4,0)</small></h2>
    <div class="row">
        <div class="col-md-6">{notaOpcional #pe3 "Prova escrita (PE3)"}</div>
        <div class="col-md-6">{notaOpcional #avf3 "Avaliação formativa (AVf3)"}</div>
    </div>

    {submitButton { label = textoDoBotao }}
|]
    where
        nota campo rotulo = (numberField campo)
            { fieldLabel = rotulo
            , additionalAttributes = [("step", "0.1")]
            }
        notaOpcional campo rotulo = (numberField campo)
            { fieldLabel = rotulo
            , required = False
            , additionalAttributes = [("step", "0.1")]
            }
