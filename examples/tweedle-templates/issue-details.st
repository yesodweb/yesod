$layout(
    title={Issue $issue.name$ -- Category $cat.name$};
    content={
<h1>$issue.name$</h1>
<h2><a href="../../category/$cat.id$/">$cat.name$</a></h2>

$if(ident)$
<form method="post" action="?_method_override=put">
<h4>Add new message</h4>
<table>
<tr><td>Status (optional)</td><td><input type="text" name="status"></td></tr>
<tr><td>Priority (optional)</td><td><input type="text" name="priority"></td></tr>
<tr><td>Description</td><td><textarea style="width:500px;height:250px" name="text"></textarea></td></tr>
<tr><td colspan="2"><input type="submit" value="Add message"></td></tr>
<table>
</form>
$else$
<form method="get" action="../../auth/openid/forward/"><h4>You must log in to add a message.</h4>
OpenID: <input type="text" name="openid"> <input type="submit" value="Login">
</form>
$endif$

<h3>Messages</h3>
$issue.messages:{message|
    <div class="message">
    <p>Author: $message.author$</p>
    <p>Created: $message.creation$</p>
    $if(message.status)$
    <p>Updated status: $message.status$</p>
    $endif$
    $if(message.priority)$
    <p>Updated priority: $message.priority$</p>
    $endif$
    <div>$message.text$</div>
    </div>
}$
})$
