$layout(
    title={Category $name$};
    content={
<ul id="breadcrumbs">
$parents:{parent|
<li><a href="../$parent.id$/">$parent.name$</a></li>
}$
</ul>
<h1>$name$</h1>

<form method="post" action="?_method_override=put">New subcategory: <input type="text" name="catname"> <input type="submit" value="Create!"></form>
<h2>Sub categories</h2>
<ul>
$cat.cats:{cat|
<li><a href="../$cat.id$/">$cat.name$</a></li>
}$
</ul>

<form method="post" action="issues/?_method_override=put">New issue: <input type="text" name="issuename"> <input type="submit" value="Create!"></form>
<h2>Issues</h2>
<table>
<thead><tr><th>Title</th><th>Status</th><th>Priority</th></tr></thead><tbody>
$cat.issues:{issue|
<tr><td><a href="../../issue/$issue.id$/">$issue.name$</a></td><td>$issue.status$</td><td>$issue.priority$</td></tr>
}$
</tbody></table>
})$
