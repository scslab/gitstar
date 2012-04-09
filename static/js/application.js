$(function() {
  $("a[href='#add_collaborator']").click(function() {
    field = $("input#new_collaborator")
    if (field.val()) {
      collab = $("<li><input type='hidden' name='collaborators[]' value='"
                + field.val() + "'/><a href='/" + field.val() + "'>" + field.val() + "</a>");
      $("ul#collaborators").append(collab);
    }
    return false;
  });

  $("a[href='#add_reader']").click(function() {
    field = $("input#new_reader")
    if (field.val()) {
      collab = $("<li><input type='hidden' name='readers[]' value='"
                + field.val() + "'/><a href='/" + field.val() + "'>" + field.val() + "</a>");
      $("ul#readers").append(collab);
    }
    return false;
  });
});
