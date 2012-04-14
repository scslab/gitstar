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

  function swapTo(elm) {
    $(".nav-pills > li").removeClass("active");
    elm.addClass("active");
  }
  
  $("a[href='#add_app']").click(function() {
    swapTo($(this).parent("li"));
    $("iframe.project_app").hide();
    $("div.project_app").show();
    return false;
  });

  $("#apps > li > a.external").click(function() {
    swapTo($(this).parent("li"));
    $("div.project_app").hide();
    $("iframe.project_app").show();
    $("iframe.project_app").attr("src", this.href);
    return false;
  });


  $("input[data-provide='typeahead']#app_search").typeahead({
    source: [{_id: "gitstar-viewer", title: "Code", url: "http://viewer.lvh.me:8081", description: "A simple code viewer for Gitstar.", name: "Code Viewer", owner: "alevy"}],
    items: 10,
    property: "name",
    onselect: function(obj) {
      console.log("hello");
      $("#app_description").html(
        "<h3>" + obj.title + "</h3>" +
        "<p class='well'>Will appear as: " + obj.title + "<br/>" +
        obj.url + "</p>" +
        "<p>" + obj.description + "</p>"
      );
      $("#new_app").val(obj["_id"]);
      $("#app_description").parent("form").show()
    }
  });
});


window.addEventListener("message", function(event) {
  $("iframe.project_app").css("height", event.data + "px");
}, false);

