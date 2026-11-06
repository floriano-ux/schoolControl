$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    espelharFlashNoConsole();
});

// Espelha as mensagens flash do servidor (login, cadastro, etc.) para o console
// do navegador, ajudando na depuração no front sem expor nada novo ao usuário.
function espelharFlashNoConsole() {
    document.querySelectorAll('.alert').forEach(function (elemento) {
        var mensagem = (elemento.textContent || '').trim();
        if (!mensagem) {
            return;
        }
        if (elemento.classList.contains('alert-danger')) {
            console.error('[schoolControl] ' + mensagem);
        } else if (elemento.classList.contains('alert-warning')) {
            console.warn('[schoolControl] ' + mensagem);
        } else {
            console.info('[schoolControl] ' + mensagem);
        }
    });
}
