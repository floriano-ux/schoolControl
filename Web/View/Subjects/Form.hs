module Web.View.Subjects.Form (renderForm) where
import Web.View.Prelude

renderForm :: Subject -> [Course] -> Html -> Html
renderForm subject courses textoDoBotao = formFor subject [hsx|
    {(selectField #courseId courses) { fieldLabel = "Curso", placeholder = "Selecione o curso" }}
    {(textField #name) { fieldLabel = "Nome" }}
    {(numberField #workload) { fieldLabel = "Carga horária (horas)" }}
    {submitButton { label = textoDoBotao }}
|]
