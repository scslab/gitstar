$(function() {
  /* Add/remove collaborators*/
  $("a[href='#add_collaborator']").click(function() {
    field = $("input#new_collaborator")
    if (field.val()) {
      collab = $("<li>"
                + "<input type='hidden' name='collaborators[]' value='"
                  + field.val() + "'/>"
                + "<a href='/" + field.val() + "'>" + field.val() + "</a> "
                + "<a href='#rm_collaborator' data-name='" + field.val() + "'>"
                  + "<span class='icon-minus'></span></a>"
                + "</li>");
      $("ul#collaborators").append(collab);
      $("a[href='#rm_collaborator']").click(function() {
        $("input[name='collaborators[]'][value='" + $(this).data("name") + "']").parent().remove()
        return false;
      });
    }
    return false;
  });

  $("a[href='#rm_collaborator']").live("click", function() {
    $("input[name='collaborators[]'][value='"
      + $(this).data("name") + "']").parent().remove()
    return false;
  });

  /* Add/remove readers*/
  $("a[href='#add_reader']").click(function() {
    field = $("input#new_reader")
    if (field.val()) {
      reader = $("<li>"
                + "<input type='hidden' name='readers[]' value='"
                  + field.val() + "'/>"
                + "<a href='/" + field.val() + "'>" + field.val() + "</a> "
                + "<a href='#rm_reader' data-name='" + field.val() + "'>"
                  + "<span class='icon-minus'></span></a>"
                + "</li>");
      $("ul#readers").append(reader);
    }
    return false;
  });

  $("a[href='#rm_reader']").live("click", function() {
    $("input[name='readers[]'][value='"
      + $(this).data("name") + "']").parent().remove()
    return false;
  });

  /* Handle public vs. readers */
  $("input[type='checkbox'][name='public']").change( function () {
    isPub = $(this).is(':checked');
    $("#new_reader").prop('disabled',isPub);
    if( isPub) {
      $("ul#readers").each(function () { $(this).remove() });
    }
    return false;
  });

  /* Handle key deletion */
  $("a[href='#del_key']").live("click", function() {
    form = $("#del_keys")
    form.append("<input type='hidden' name='_delete' value='"
                + $(this).data("key") + "'/>");
    form.submit()
    return false;
  });


  /* Handle apps */
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
    source: function(typeahead, query) {
      return ($.getJSON("/apps", {q: query},
        function(data) {
            return typeahead.process(data);
          }
        ));
    },
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

