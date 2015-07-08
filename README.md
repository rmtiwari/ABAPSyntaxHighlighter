ABAPSyntaxHighlighter (PrismABAP):
=====================

It's a lightweight, syntax highlighter for SAP's programming language ABAP and it's written in JavaScript - ( Duh!....ABAP is not written in JavaScript but just this syntax highlighter ) <br>
This is an extension of open source Syntax Highlighter [Prism](http://www.prismjs.com).


### Why ABAPSyntaxHighlighter ( PrismABAP )?
------------------------------------------------------------------------------------------------------------------------
I wanted to put together a JavaScript library that can achieve effective syntax-highlighting for ABAP Code.
ABAPSyntaxHighlighter ( PrismABAP ) offers a practical solution with acceptable margins of error. Goal is to have an easy to install and useable JavaScript Library for the purpose.

### How to use ABAP syntax-highlighter on a website / blog:
------------------------------------------------------------------------------------------------------------------------
* Site/Blog Administrator/owner should add the [JavaScript](http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.js) and [CSS](http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.css) files as below (both files can be downloaded from this repository). Obviously this is one time activity.<br />

  ```html
<head>
....
<link href='http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.css' rel='stylesheet'/>
<script src='http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.js'></script>
....
</head>
  ```

* Post writer will enclose the ABAP code with `<pre>` and `<code>` and specify the CSS class name: line-numbers language-abap as below:

  ```html
<pre class="line-numbers language-abap"><code>
* Actual ABAP Code lines should be pasted here - site/blog post editor should take care of converting any starting html brackets in ABAP code to &amp;lt; for example field-symbols etc.
</code></pre>
  ```

A sample highlighted code and some details are available at [this Blog Post](http://sapblog.rmtiwari.com/2014/01/hacking-together-abap-syntax.html).
If you want to try on your Blog/site – it’s free unless you are hell-bent on paying for it :)
