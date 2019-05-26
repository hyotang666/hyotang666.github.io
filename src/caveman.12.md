<!-- {% raw %} -->
# Caveman kills ruby on rails - Chapter 12
## Meta info
### 対象読者
* Cavemanアプリにjavascriptを導入したいCLer

## Introduction
本稿は[原著](https://book.impress.co.jp/books/1117101135)の各章をCommon Lispに翻訳するシリーズの第12章である。
Cavemanにはjavascript周りをいい具合に導入できるようなサポートはない。
原始的で愚直な最低限の事のみを行う。

よって12.1、12.2、12.3章はTODOとして無視する。

## 12.1 credentials
## 12.2 asset pipeline
## 12.3 Sass

## 12.4 javascript
Cavemanに於いてjavascriptを書く場合、ソースはデフォルトではstatic/js/下に配置することとなる。

### jQuery
いい具合に導入できたりはしない。
ここではgoogleのCDNを利用することとする。

#### templates/layouts/app.html

```html
<head>
        <meta charset="utf-8">
        <title>{% block title %}Your-app{% endblock %}</title>
        <link rel="stylesheet" type="text/css" media="screen" href="/css/app.css">
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.0/jquery.min.js"></script>
</head>
```

### extends article form.
jsコードを以下のように作成。

#### static/js/articles.js

```js
$(document).ready(function(){
        var cb = $("#no-expiration");
        var field = $("#article-expired-at");

        var changeExpiredAt = function(){
                if (cb.prop("checked"))
                        field.hide()
                else
                        field.show()
        }

        cb.bind("click", changeExpiredAt);

        changeExpiredAt();
});
```
scriptタグを追加。

#### templates/articles/new.html

```html
{% block content %}
<script src="/js/articles.js"></script>
```

## Summary
* Javascriptを導入したい場合はstatic/js下に配置してロードします。
<!-- {% endraw %} -->
