$(document).ready(function () {
    $("form").submit(function (e) {
        e.preventDefault();
        $.getJSON("/post", { name: $("#name").attr("value"), send: $("#send").attr("value") }, function(o) { });
        $("#send").attr("value", "");
    });

    checkIn();

});

function checkIn () {
    $.getJSON("/check", { client: clientNumber }, function(o) {
        //alert("response: " + o);
        var ta = $("textarea");
        ta.html(ta.html() + o.sender + ": " + o.content + "\n");
        ta.scrollTop(10000);

        checkIn();
    });
}
