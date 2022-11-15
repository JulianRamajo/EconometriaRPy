---
editor_options: 
  markdown: 
    wrap: 72
---

# Markdown y $\LaTeX$

> Reglas básicas

## 1. Markdown

Se trata de un lenguaje de composición de textos, y mediante una
sintaxis sencilla sintaxis se agrega formato al texto.

En una misma línea, lo que siga a uno varios símbolos `#` se convierte
en encabezado. Entre más símbolos, de menor nivel (tamaño) será el
encabezado:

# Encabezado nivel 1

## Encabezado nivel 2

### Encabezado nivel 3

Para resaltar en formato código se utiliza un acento grave como apertura
y otro igual como cierre, por ejemplo: `Julia`

Para convertir texto a formato en *itálicas* basta encerrar el texto
mediante símbolos de asterisco `*`

Para convertir texto a formato en **negritas** basta encerrar el texto
mediante doble asterisco `**` en la apertura y luego nuevamente `**` al
cierre.

Con apertura y cierre mediante triple asterisco `***` se obtiene
***negritas en itálicas***.

Para iniciar un nuevo renglón dentro de la misma celda, es necesario
dejar dos espacios antes de generar el nuevo renglón, de lo contrario
aparecerá en forma seguida.\
Si queremos generar un nuevo párrafo dentro de una misma celda, debemos
dejar un renglón vacío que los separe.

Aquí inicia el siguiente párrafo.

Formato especial para citas:

> Esta es una cita.

El formato de cita puede anidarse, es decir, tener varios niveles, citas
dentro de las citas:

> Este es el primer nivel de cita.

> > Y aquí una cita dentro de la cita.

Para generar líneas divisorias horizontales ocupar 3 o más símbolos
`___` (underscore), dejando siempre un renglón en blanco antes y
después. Por ejemplo:

Antes de la línea

------------------------------------------------------------------------

Después de la línea

Bloques de código de programación dentro del entorno de Markdown
mediante triple acento grave a la apertura y cierre del bloque. Por
ejemplo:

    n = 20
    for d ∈ 0:n
        print(d, " ")
    end

Si queremos agregar color al código de acuerdo con la sintaxis de Julia,
solo hay que agregar `julia` después del triple acento grave a la
apertura:

``` julia
n = 20
for d ∈ 0:n
    print(d, " ")
end
```

### Listas

Las hay ordenadas y no ordenadas. Para el caso de **lista ordenada** se
utiliza numeración arábiga:

1.  La primera

2.  La segunda

3.  La tercera

Aunque se intente utilizar otra numeración, Markdown asignará la
numeración de forma consecutiva, iniciando con el primer número que se
propocione:

8.  Escribí 8
9.  Escribí 5
10. Escribí 99

Es posible crear **sublistas ordenadas** dentro de una lista, mediante
indentación: 1. Primera 2. Segunda 1. primera 2. segunda 3. Tercera 1.
primera 1. prim 2. seg 1. pr 2. se 2. segunda 4. Cuarta

Para crear **listas no ordenadas** se utilizan los símbolos `-` `*` `+`
indistintamente, pero no los combine porque esto puede generar errores
en ocasiones:

-   ítem
-   ítem siguiente
-   etc.

También es posible tener **sublistas no ordenadas** mediante
indentación:

-   ítem
    -   subítem
    -   subítem siguiente
        -   subsubítem
        -   subsubítem siguiente
        -   etc.
    -   etc.
-   ítem siguiente
-   etc.

Combinar listas ordenadas y no ordenadas:

1.  Primera
    -   ítem
    -   ítem siguiente
        1.  perro
        2.  gato
    -   etc.
2.  Segunda
3.  Bye

En una lista (ordenada o no) se pueden intercalar párrafos o citas:

1.  Primera
2.  Segunda \> Comentario
3.  Tercera
4.  etc.

### Tablas

Abrir la celda siguiente para ver la sintaxis:

| columna 1 | columna 2 | columna 3 |
|-----------|-----------|-----------|
| valor 11  | valor 12  | valor 13  |
| valor 21  | valor 22  | valor 23  |
| valor 31  | valor 32  | valor 33  |
| valor 41  | valor 42  | valor 43  |

### Links

Para un **enlace a internet**, escribir el nombre que los describe entre
corchetes y seguido la dirección de internet entre paréntesis. Sintaxis:

> `[nombre que le doy al enlace](dirección de internet)`

Y así se ve un ejemplo:

[Página oficial de Julia](https://julialang.org/)

Se puede agregar estilo de negritas, itálicas o tipo código a un enlace.
Por ejemplo:

[***Julia Computing
Inc.***](https://juliacomputing.com "Julia Computing")

## 2. $\LaTeX$

Es también un lenguaje de composición de textos, pero mucho más
sofisticado y poderoso, especialmente diseñado para el uso de símbolos,
de particular uso en disciplinas como Matemáticas, Física, Química y
Lingüística, entre otras. Para explotar todo el potencial de $\LaTeX$ es
necesario instalarlo por separado, pero dentro de Jupyter notebooks
funciona mucho de su sintaxis, que con frecuencia resultará más que
suficiente.

Para que `Markdown` reconozca sintaxis de $\LaTeX$ simplemente hay que
escribirla entre símbolos `$`, uno de apertura y uno de cierre si se va
a intercalar dentro de una redacción, o bien utilizándolo doble `$$` a
la apertura y cierre si se desea en un párrafo aparte y centrado.

Por ejemplo, dentro de este párrafo de texto introducimos la notación
$f(x) = 2x + 1$ y continuamos escribiendo. Si escribimos lo mismo pero
utilizando `$$` obtenemos lo siguiente: $$
f(x) = 2x + 1
$$

La diferencia puede ser más notoria dependiendo de la notación
utilizada. Un ejemplo muy claro es con el uso del símbolo de
integración, que dentro de un párrafo de texto se ve así
$\int_a^b f(x)\,dx$ pero mediante `$$` se ve así: $$\int_a^b f(x)\,dx$$

### Tipos de letras

Griegas minúsculas:

`\alpha` $\alpha$ `\beta` $\beta$ `\gamma` $\gamma$ `\delta` $\delta$
`\epsilon` $\epsilon$ `\varepsilon` $\varepsilon$ `\zeta` $\zeta$ `\eta`
$\eta$ `\theta` $\theta$ `\vartheta` $\vartheta$ `\iota` $\iota$
`\kappa` $\kappa$ `\lambda` $\lambda$ `\mu` $\mu$ `\nu` $\nu$ `\xi`
$\xi$ `\omicron` $\omicron$ `\pi` $\pi$ `\varpi` $\varpi$ `\rho` $\rho$
`\varrho` $\varrho$ `\sigma` $\sigma$ `\varsigma` $\varsigma$ `\tau`
$\tau$ `\upsilon` $\upsilon$ `\phi` $\phi$ `\varphi` $\varphi$ `\chi`
$\chi$ `\psi` $\psi$ `\omega` $\omega$

Griegas mayúsculas:

`\Gamma` $\Gamma$ `\Delta` $\Delta$ `\Theta` $\Theta$ `\Lambda`
$\Lambda$ `\Xi` $\Xi$ `\Pi` $\Pi$ `\Sigma` $\Sigma$ `\Upsilon`
$\Upsilon$ `\Phi` $\Phi$ `\Psi` $\Psi$ `\Omega` $\Omega$ `\mho` $\mho$

Caligráficas mayúsculas:

`\mathcal{A}` $\mathcal{A}$ `\mathcal{B}` $\mathcal{B}$ ...
`\mathcal{Y}` $\mathcal{Y}$ `\mathcal{Z}` $\mathcal{Z}$

Bold mayúsculas:

`\mathbb{A}` $\mathbb{A}$ `\mathbb{B}` $\mathbb{B}$ ... `\mathbb{Y}`
$\mathbb{Y}$ `\mathbb{Z}` $\mathbb{Z}$

### Miscelánea

`\forall` $\forall$ `\exists` $\exists$ `\neg` $\neg$ `\infty` $\infty$
`\emptyset` $\emptyset$ `\varnothing` $\varnothing$ `\partial`
$\partial$ `\nabla` $\nabla$ `\surd` $\surd$

`\angle` $\angle$ `\top` $\top$ `\bot` $\bot$ `\triangle` $\triangle$
`\square` $\square$ `\diamond` $\diamond$ `\circ` $\circ$ `\bigcirc`
$\bigcirc$ `\aleph` $\aleph$ `\ell` $\ell$

### Operadores binarios

`\pm` $\pm$ `\mp` $\mp$ `\times` $\times$ `\div` $\div$ `\ast` $\ast$
`\star` $\star$ `\circ` $\circ$ `\bullet` $\bullet$ `\cdot` $\cdot$

`\oplus` $\oplus$ `\ominus` $\ominus$ `\otimes` $\otimes$ `\oslash`
$\oslash$ `\odot` $\odot$

`\vee` $\vee$ `\wedge` $\wedge$ `\wr` $\wr$ `\cap` $\cap$ `\cup` $\cup$
`\uplus` $\uplus$ `\sqcap` $\sqcap$ `\sqcup` $\sqcup$ `\setminus`
$\setminus$

`\bigtriangleup` $\bigtriangleup$ `\bigtriangledown` $\bigtriangledown$
`\triangleleft` $\triangleleft$ `\triangleright` $\triangleright$

### Operadores grandes

`\sum` $\sum$ `\prod` $\prod$ `\coprod` $\coprod$ `\int` $\int$ `\oint`
$\oint$

`\bigoplus` $\bigoplus$ `\bigotimes` $\bigotimes$ `\bigodot` $\bigodot$

`\bigcap` $\bigcap$ `\bigcup` $\bigcup$ `\biguplus` $\biguplus$
`\bigvee` $\bigvee$ `\bigwedge` $\bigwedge$ `\bigsqcup` $\bigsqcup$

### Relaciones binarias

`\leq` $\leq$ `\geq` $\geq$ `\prec` $\prec$ `\succ` $\succ$ `\sim`
$\sim$ `\preceq` $\preceq$ `\succeq` $\succeq$ `\simeq` $\simeq$ `\ll`
$\ll$ `\gg` $\gg$

`\neq` $\neq$ `\doteq` $\doteq$ `\approx` $\approx$ `\cong` $\cong$
`\propto` $\propto$ `\equiv` $\equiv$ `\not\equiv` $\not\equiv$

`\subset` $\subset$ `\supset` $\supset$ `\subseteq` $\subseteq$
`\supseteq` $\supseteq$ `\sqsubset` $\sqsubset$ `\sqsupset` $\sqsupset$
`\sqsubseteq` $\sqsubseteq$ `\sqsupseteq` $\sqsupseteq$ `\in` $\in$
`\ni` $\ni$ `\notin` $\notin$

`\vdash` $\vdash$ `\dashv` $\dashv$ `\perp` $\perp$ `\parallel`
$\parallel$ `\models` $\models$ `\smile` $\smile$ `\frown` $\frown$

### Flechas

`\leftarrow` $\leftarrow$ `\longleftarrow` $\longleftarrow$ `\Leftarrow`
$\Leftarrow$ `\Longleftarrow` $\Longleftarrow$

`\rightarrow` $\rightarrow$ `\longrightarrow` $\longrightarrow$
`\Rightarrow` $\Rightarrow$ `\Longrightarrow` $\Longrightarrow$

`\leftrightarrow` $\leftrightarrow$ `\longleftrightarrow`
$\longleftrightarrow$ `\Leftrightarrow` $\Leftrightarrow$
`\Longleftrightarrow` $\Longleftrightarrow$

`\uparrow` $\uparrow$ `\Uparrow` $\Uparrow$ `\downarrow` $\downarrow$
`\Downarrow` $\Downarrow$ `\updownarrow` $\updownarrow$ `\Updownarrow`
$\Updownarrow$

`\mapsto` $\mapsto$ `\longmapsto` $\longmapsto$ `\hookrightarrow`
$\hookrightarrow$ `\hookleftarrow` $\hookleftarrow$ `\leadsto`
$\leadsto$

`\nwarrow` $\nwarrow$ `\nearrow` $\nearrow$ `\searrow` $\searrow$
`\swarrow` $\swarrow$

### Delimitadores

`\{` $\{$ `\}` $\}$ `\lfloor` $\lfloor$ `\rfloor` $\rfloor$ `\lceil`
$\lceil$ `\rceil` $\rceil$ `\langle` $\langle$ `\rangle` $\rangle$

`\lmoustache` $\lmoustache$ `\rmoustache` $\rmoustache$ `\lgroup`
$\lgroup$ `\rgroup` $\rgroup$

### Guías rápidas

> <https://www.math.ubc.ca/~pwalls/math-python/jupyter/latex/>

> <https://en.wikibooks.org/wiki/LaTeX/Mathematics>

### Ejemplos

`Para resolver una integral del tipo $$\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx$$ se requiere...`

Para resolver una integral del tipo
$$\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx$$ se requiere...

`Para resolver una integral del tipo $\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx$ se requiere...`

Para resolver una integral del tipo
$\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx$ se requiere...

`Para resolver una integral del tipo $\displaystyle{\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx}$ se requiere...`

Para resolver una integral del tipo
$\displaystyle{\int_{a}^{b}\sqrt{1 + \hat{f}(x)}\,dx}$ se requiere...

`$\hat{x} \quad \widehat{x} \qquad \bar{x} \qquad \tilde{x} \qquad \widetilde{x} \qquad \vec{x} \qquad \dot{x} \qquad \sqrt{x}$`

$\hat{x} \quad \widehat{x} \qquad \bar{x} \qquad \tilde{x} \qquad \widetilde{x} \qquad \vec{x} \qquad \dot{x} \qquad \sqrt{x}$

`$$\sqrt{\tilde{\alpha}_{1}^{2} + \dot{\beta}_{1}^{\delta^{2}}}$$`

$$\sqrt{\tilde{\alpha}_{1}^{2} + \dot{\beta}_{1}^{\delta^{2}}}$$

`$$\sqrt{1 + \sqrt{1 + \sqrt{1 + \sqrt{1 + \delta}}}}$$`

$$\sqrt{1 + \sqrt{1 + \sqrt{1 + \sqrt{1 + \delta}}}}$$

`\Gamma \stackrel{f}{\longrightarrow} \Psi \stackrel{g}{\longrightarrow} \Omega`

$$\Gamma \stackrel{f}{\longrightarrow} \Psi \stackrel{g}{\longrightarrow} \Omega$$

Se recomienda el uso de `\exp \log ...` esto es, por ejemplo
$\exp(-x^2)$ en lugar de $exp(x^2)$

`Usar $\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$ versus $$\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$$`

Usar $\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$ versus
$$\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$$

Aunque siempre se puede forzar el `displaystyle` por ejemplo:

`Usar $\displaystyle{\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}}$ versus $$\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$$`

Usar $\displaystyle{\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}}$
versus $$\bigcup_{k=1}^n\{k:k^2\leq\frac{1}{2}\alpha_k\}$$

`$x_1,\ldots, x_n \qquad x_1 + \cdots + x_n$`

$x_1,\ldots, x_n \qquad x_1 + \cdots + x_n$

`$$\underline{x} \qquad \overline{\alpha + \beta} \qquad{\underbrace{a + b + \cdots + z}}$$`

$$\underline{x} \qquad \overline{\alpha + \beta} \qquad{\underbrace{a + b + \cdots + z}}$$

`$$\underbrace{a + \overbrace{b + \cdots + y}^{24} + z}_{26}$$`

$$\underbrace{a + \overbrace{b + \cdots + y}^{24} + z}_{26}$$

`$($ $\big($ $\Big($ $\bigg($ $\Bigg($ y lo análogo para $), [, ], \{, \}$`

$($ $\big($ $\Big($ $\bigg($ $\Bigg($ y lo análogo para
$), [, ], \{, \}$

O bien ajustar automáticamente mediante `\left` y `\right`:

`$$\sqrt{1 + \left(\int_a^b \frac{f(x)}{1 + g(x)}\,dx\right)^2}$$`

$$\sqrt{1 + \left(\int_a^b \frac{f(x)}{1 + g(x)}\,dx\right)^2}$$

Espacios:

Ninguno: `$2x$` $2x$

Poco: `$2\,x$` $2\,x$

Algo: `$2\quad x$` $2\quad x$

Mucho: `$2\qquad x$` $2\qquad x$

En ocasiones queremos que las expresiones se acerquen:
`$$\int_a^b \int_c^d \sqrt{1 + f(x,y)}\,dxdy$$`
$$\int_a^b \int_c^d \sqrt{1 + f(x,y)}\,dxdy$$

Esto se logra mediante `\!` por ejemplo:
`$$\int_a^b\!\!\!\int_c^d\!\!\!\!\!\sqrt{1 + f(x,y)}\,dxdy$$`
$$\int_a^b\!\!\!\int_c^d\!\!\!\!\!\sqrt{1 + f(x,y)}\,dxdy$$

Ecuación en varias líneas:

    $$\begin{align}
        F(x) & = \int_{0}^{\,x}e^{-t}\,dt \\
             & = -e^{-t}\Big|_{\,t=0}^{\,t=x} \\
             & = 1 - e^{-x}
      \end{align}$$

$$\begin{align}
    F(x) & = \int_{0}^{\,x}e^{-t}\,dt \\
         & = -e^{-t}\Big|_{\,t=0}^{\,t=x} \\
         & = 1 - e^{-x}
  \end{align}$$

Matrices `r = right` `l = left` `c = center`

    $$\mathbb{A} = \left( \begin{array}{lcr}
                             1 &   2 &   3 \\
                            10 &  20 &  30 \\
                           100 & 200 & 300 
                           \end{array} \right)$$

$$\mathbb{A} = \left( \begin{array}{lcr}
                         1 &   2 &   3 \\
                        10 &  20 &  30 \\
                       100 & 200 & 300 
                       \end{array} \right)$$
