/*
 * jQuery Thead Plugin v1.1
 * http://www.asual.com/jquery/thead/
 *
 * Copyright (c) 2009-2010 Rostislav Hristov
 * Dual licensed under the MIT or GPL Version 2 licenses.
 * http://jquery.org/license
 *
 * Date: 2010-09-26 19:36:34 +0300 (Sun, 26 Sep 2010)
 */
(function(a){var k=navigator.userAgent,o=a(window),l=a(document),i=[],m=!(a.browser.msie&&parseFloat(k.substr(k.indexOf("MSIE")+4))<7),f=null,n=function(b){b=parseInt(b,10);return isNaN(b)?0:b},h=function(){a(i).each(function(){var b,c=a("table",this.parent().prev()).get(0),g=a("caption",c);a(c).css("border-collapse");var j=a("thead tr th, thead tr td",c),d=l.scrollTop()-a(c).offset().top+4;if(g.length)d-=g.get(0).clientHeight;a("thead tr th, thead tr td",this).each(function(e){e=j.eq(e).get(0);b=
a(e).css("width");a(this).css("width",b!="auto"?b:e.clientWidth-n(a(e).css("padding-left"))-n(a(e).css("padding-right"))+"px")});a(this).css({display:d>4&&d<c.clientHeight-a("tr:last",c).height()-8?a(c).css("display"):"none",left:a(c).offset().left-l.scrollLeft()+"px",width:a(c).get(0).offsetWidth})})};a(function(){if(m){o.scroll(h).resize(function(){if(f===null)f=setInterval(function(){if(f)f=clearInterval(f);h()},50)});a(document).ajaxComplete(function(){a.thead.update()});a(".jquery-thead").thead()}});
a.thead=function(){return{update:function(){a(i).each(function(){var b=a("thead",a("table.jquery-thead, table",this.parent().prev()).get(0)),c=a("thead",this);if(c.html()!=b.html()){c.parent().append(b.clone(true));c.remove();h()}})}}}();a.fn.thead=function(){m&&a(this).each(function(){var b=this.tagName.toLowerCase()=="table"?a(this):a("table",this),c=b.parent(),g=a("thead",b);if(g.length){var j=b.attr("class"),d=b.attr("cellpadding");b=b.attr("cellspacing");i.push(a("<table />").attr("class",j).attr("cellpadding",
d?d:1).attr("cellspacing",b?b:2).css({position:"fixed",top:0}).appendTo(a("<"+c.get(0).tagName+"/>").attr("class",c.attr("class")).insertAfter(c)).append(a(g).clone(true)))}});h()}})(jQuery);
