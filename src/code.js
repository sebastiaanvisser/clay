$(window).scroll
  ( function ()
    {
      $( "header" ).css
        ( "top"
        , Math.max(-280, Math.min(0, -$($.browser.mozilla ? window : "body").scrollTop()))
        );
    }
  );

$("a").click
  ( function (ev)
    {
      if ($(ev.target).attr("href")[0] != "#") return;
      var target = $("*[name=" + $(ev.target).attr("href").substr(1) + "]");

      $("html, body").animate
        ( { scrollTop : $(target).offset().top - 80 }
        , ev.shiftKey ? 2500 : 500
        );
      ev.preventDefault();
      ev.stopPropagation();
    }
  );

$("header > div").click
  ( function (ev)
    {
      $("html, body").animate({ scrollTop: 0 });
    }
  );

