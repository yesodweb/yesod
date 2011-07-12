$(function(){
  $("#navbar a").click(function(){
    $.getJSON($(this).attr("href"), {}, function(o){
      $("h1").html(o.name);
      $("article").html(o.content);
    });
    return false;
  });
});
