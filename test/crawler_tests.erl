-module(crawler_tests).
-include_lib("eunit/include/eunit.hrl").

extract_links_test() ->
    Body = "
<link href=\"http://yandex.st/highlightjs/7.5/styles/default.min.css\" media=\"screen\" rel=\"stylesheet\" type=\"text/css\" />
<a class=\"brand\" href=\"/\">Nox73.ru</a>
<li><a href=\"/memory\">Memory</a></li>
<li><a href=\"/notes\">Notes</a></li>
<a class=\"dropdown-toggle\" data-toggle=\"dropdown\" href=\"#\">
<i class=\"icon-user icon-large\"></i>
Sign In
<b class=\"caret\"></b>
</a>
<a href=\"/user/twitter/authorize\">
<a href=\"https://twitter.com/NOX_73\">@NOX_73</a>
<a href=\"https://www.facebook.com/nox73\">nox73</a>
<a href=\"https://github.com/NOX73\">NOX73</a>
<a href=\"https://coderwall.com/nox73\">
<img alt=\"Endorse NOX73 on Coderwall\" src=\"http://api.coderwall.com/nox73/endorsecount.png\">
</a>
<a href=\"http://avrozhnov.moikrug.ru/?from=userlink\">
<img alt=\"Alexander Rozhnov\" src=\"http://moikrug.ru/images/icon.gif\" style=\"width: 16px; height: 16px; border:0; vertical-align: middle; margin-right: 3px;\" title=\"Alexander Rozhnov\">
</a>
<a href=\"http://avrozhnov.moikrug.ru/?from=userlink\" style=\"font-size: 26px\" title=\"Alexander Rozhnov\"><span style=\"color: #C71717\">A</span>lexander Rozhnov
",

ExpLinks = ["http://nox73.ru/memory","http://nox73.ru/notes", "http://nox73.ru/user/twitter/authorize"],
Links = crawler:extract_links("nox73.ru" , Body),
?assertEqual(ExpLinks,Links).


    