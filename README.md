ABAPSyntaxHighlighter (PrismABAP):
=====================

It's is a lightweight, syntax highlighter for SAP's programming language ABAP and it's written in JavaScript - ( Duh!....ABAP is not written in JavaScript but just this syntax highlighter )
This is an extension of open source Syntax Highlighter <a href="http://www.prismjs.com">Prism</a>.


<h3>Why ABAPSyntaxHighlighter ( PrismABAP )?</h3>
------------------------------------------------------------------------------------------------------------------------
I wanted to put together a JavaScript library that can achieve effective syntax-highlighting for ABAP Code.
ABAPSyntaxHighlighter ( PrismABAP ) offers a practical solution with acceptable margins of error. Goal is to have an easy to install and useable JavaScript Library for the purpose.

<h3>How to use ABAP syntax-highlighter on a website / blog: </h3>
------------------------------------------------------------------------------------------------------------------------
<ul><li>Site/Blog Administrator/owner should add the <a href="http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.js" target="_blank">JavaScript</a> and <a href="http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.css" target="_blank">CSS</a> files as below (both files can be downloaded from this repository). Obviously this is one time activity.<br />
<pre class="prettyprint">&lt;head&gt;
....
&lt;link href='http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.css' rel='stylesheet'/&gt;
&lt;script src='http://www.pragmatiqa.com/docs/ABAPSyntaxHighlighter/Final/prismABAP.js'&gt;&lt;/script&gt;
....
&lt;/head&gt;

</pre></li>
<li>Post writer will enclose the ABAP code with &lt;pre&gt; and &lt;code&gt; and specify the CSS class name: line-numbers language-abap as below:</li>
<pre class="prettyprint">&lt;pre class="line-numbers language-abap"&gt;&lt;code&gt;
* Actual ABAP Code lines should be pasted here  &#8211; site/blog post editor should take care of converting any starting html brackets in ABAP code to &amp;lt; for example field-symbols etc.
&lt;/code&gt;&lt;/pre&gt;
</pre><li>
A sample highlighted code and some details are available at <a href="http://sapblog.rmtiwari.com/2014/01/hacking-together-abap-syntax.html">this Blog Post</a>. 
If you want to try on your Blog/site – it’s free unless you are hell-bent on paying for it :)
</li>
</ul>
