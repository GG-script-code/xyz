HTML_Scripts<<-list()

# Divergenze ----
HTML_Scripts[["Divergenze"]]<-paste0("<p>Le divergenze sono degli indicatori chiave dell&rsquo;analisi tecnica e si intende una situazione in cui il movimento dei prezzi diverge da quello di un indicatore. Grazie alle divergenze, infatti, &egrave; possibile comprendere meglio come si muovono i mercati.</p>

<p>Le divergenze si basano su tre indicatori di analisi tecnica che servono a valutare la forza di un trend in corso: &ldquo;l&rsquo;indice di forza relativa&rdquo; (RSI), la &ldquo;convergenza e divergenza della media mobile&rdquo; (MACD) e &ldquo;l&rsquo;oscillatore stocastico&rdquo;.</p>

<p>Puo' accadere che i grafici dei prezzi e gli indicatori di analisi tecnica offrano segnali contrastanti (divergenze) e facendo una valutazione pi&ugrave; completa &egrave; possibile identificare differenti tipi di divergenze:</p>

<dl>
<dt><b>Intensit&agrave; del trend</b>:</dt>

<dd> - &ldquo;regolare&rdquo; indica che un trend potrebbe invertirsi o arrestarsi;</dd>

<dd> - &ldquo;nascosta&rdquo; segnala che un trend tender&agrave; a continuare nonostante il grafico dei prezzi mostri segni di indebolimento;</dd>

<dt><b>Direzione del trend</b>:</dt>

<dd> - &ldquo;positiva&rdquo; indica un probabile aumento dei prezzi;</dd>

<dd> - &ldquo;negativa&rdquo; segnala una possibile fase ribassista.</dd>
</dl>

<p>Per identificare una divergenza occorre confrontare e sovrapporre il grafico dei prezzi dell&rsquo;asset scelto, con l&rsquo;indicatore di divergenza che si desidera utilizzare. <br />
Per quanto concerne le <b>divergenze regolari</b>, se il prezzo di un asset forma minimi decrescenti mentre l&rsquo;indicatore tecnico mostra minimi crescenti, ci&ograve; potrebbe indicare un modello &ldquo;bullish&rdquo;, ossia rialzista. In questo caso, si parla proprio di &ldquo;divergenza regolare rialzista&rdquo;. Se, invece, il prezzo di un asset forma massimi crescenti mentre l&rsquo;indicatore tecnico segnala massimi decrescenti, ci&ograve; potrebbe indicare che il trend si sta affievolendo e che potrebbe esservi un movimento &ldquo;bearish&rdquo;, ossia ribassista. In questa seconda situazione si parla di &ldquo;divergenza regolare ribassista&rdquo;.<br />
Le <b>divergenze nascoste</b>, invece, segnalano una probabile continuazione di un trend in corso. In particolare, se il prezzo forma un minimo crescente, ma l&rsquo;indicatore tecnico segnala un minimo decrescente, avremo una divergenza nascosta rialzista, ossia l&rsquo;indicazione di un possibile indebolimento della tendenza al rialzo. Se, invece, il prezzo forma un massimo decrescente mentre l&rsquo;indicatore tecnico segnala un massimo crescente, avremo una divergenza nascosta ribassista, ossia un probabile indebolimento della tendenza al ribasso.</p>

<p>Il primo errore da evitare quando si usano le divergenze, &egrave; quello di essere troppo avventati. Nel caso delle divergenze regolari, per esempio, pu&ograve; passare del tempo prima che il trend si inverta significativamente. &Egrave; molto importante, inoltre, considerare la forza del segnale. Altro errore da evitare, &egrave; trascurare l&rsquo;analisi complessiva del mercato. Le strategie di trading basate sulle divergenze, infatti, necessitano di un monitoraggio costante delle condizioni del mercato. <br />
Quando si utilizzano le divergenze, inoltre, &egrave; importante assicurarsi che gli strumenti di analisi siano impostati correttamente. I grafici degli indicatori possono essere adattati a diversi intervalli di tempo e basarsi su dati provenienti da diverse fonti, come le medie mobili semplici, le medie mobili esponenziali o le medie mobili ponderate.</p>")


# RSI ----
HTML_Scripts[["RSI"]]<-paste0("<p>Il <a href='https://www.ig.com/it/strategie-di-trading/oscillatore-rsi--cose-e-come-utilizzarlo-nel-trading-200814'> Relative Strength Index (RSI)</a> calcola un rapporto tra i recenti movimenti di prezzo al rialzo e il movimento di prezzo assoluto.</p>

<p>Il calcolo dell'RSI &egrave; RSI=100-100/(1+RS), dove RS &egrave; il rapporto levigato dei guadagni &quot;medi&quot; sulle perdite &quot;medie&quot;. Le &quot;medie&quot; non sono vere medie, poich&eacute; sono divise per il valore di n e non per il numero di periodi in cui ci sono guadagni/perdite.</p>

<p>L'RSI &egrave; solitamente interpretato come un indicatore di ipercomprato/ipervenduto (oltre 70/sotto 30). Anche la linea del 50 &egrave; un punto critico, e puo' indicare che il prezzo salir&agrave;, quando dall'oversold sale e supera questa linea, o che scender&agrave;, quando dall'overbought scende e la supera.</p>

<p>L'identificazione di picchi ripetuti, in alto o in basso, possono anticipare una salita/discesa di prezzo repentina nel breve periodo (in base al time frame di riferimento).</p>

<p>Altro segnale importante possono essere le divergenza, ossia quando in un range di periodi consistente (circa 25) l'andamento del RSI diverge da quello dei prezzi, con un RSI in discesa/salita ed un prezzo in salita/discesa. Probabilmente il RSI sta anticipando un andamento futuro dei prezzi (in discesa/salita).</p>")

# Ichimoku ----
HTML_Scripts[["Ichimoku"]]<-paste0("<p>La &quot;strategia&quot; Ichimoku &egrave; un indicatore tecnico utilizzato per valutare lo slancio, insieme alle aree future di supporto e resistenza. La &quot;nuvola&quot; che si former&agrave; tra queste linee prende il nome di __Senkou Span__. Questo indicatore &egrave; formato da diverse linee:</p>

<p> 1. La Tenkan-Sen, chiamata anche Conversion Line, segnala i minimi cambi e la resistenza e rappresenta il punto medio degli ultimi 9 candelieri. Viene calcolata sommando il massimo pi&ugrave; alto e il minimo pi&ugrave; basso degli ultimi nove periodi e dividendo il risultato per due.</p>

<p> 2. La linea Kijun-Sen, chiamata anche Base Line, &egrave; la linea di conferma che pu&ograve; essere usata per analizzare il momentum del prezzo di mercato e rappresenta il punto medio degli ultimi 26 candelieri. Viene calcolata in modo simile alla linea Tenkan-Sen, tuttavia utilizziamo gli ultimi 26 candelieri anzich&eacute; gli ultimi 9.</p>

<p> 3. Senkou Span A, chiamato anche Leading Span A, rappresenta uno dei due confini del Cloud ed &egrave; il punto medio tra la Conversion Line (Tenkan-Sen) e la Base Line (Kijun-Sen). Viene calcolato sommando la linea Tenkan-Sen e la linea Kijun-Sen e dividendo per 2. Questo valore &egrave; tracciato 26 periodi nel futuro ed &egrave; il margine superiore delle due nuvole.</p>

<p> 4. Senkou Span B, o Leading Span B, rappresenta i secondi confini del Cloud ed &egrave; il punto medio delle ultime 52 barre dei prezzi. Aggiungi il massimo pi&ugrave; alto e il minimo pi&ugrave; basso negli ultimi 52 periodi e poi dividi il risultato per due. Questo valore &egrave; tracciato 26 periodi nel futuro e forma il margine inferiore della nuvola.</p>

<p> 5. Chikou Span, chiamato anche Lagging Span, &egrave; in ritardo rispetto al prezzo (come suggerisce il nome). Il Lagging Span &egrave; tracciato 26 periodi indietro e presenta il sentimento di mercato tracciando i livelli di chiusura degli ultimi 26 giorni. Fornisce un'istantanea del prezzo di mercato attuale in rapporto alle tendenze storiche. Pu&ograve; servire anche per individuare potenziali inversioni.</p>

<p>Si usa la linea di base per determinare il momentum di prezzo a breve termine. L'incrocio della Tenkan Sen (linea di conversione) con la Kijun Sen (linea di base) indica che il momentum del livello di prezzo &egrave; in aumento. Quando la Tenkan Sen si trova sopra la Kijun Sen, indica un segnale di acquisto. Se avviene il contrario, si tratta di un segnale di vendita.<br />
Se il livello di prezzo &egrave; superiore alla linea di base (Kijun Sen), significa che il prezzo di mercato &egrave; orientato al rialzo, poich&eacute; supera il prezzo intermedio di un intervallo di 26 periodi. In questo caso, la nuvola si considera una barriera di supporto o di resistenza.</p>

<p>Quando la Chikou Span, che indica il sentimento di mercato, mostra una flessione nel prezzo dell'asset, significa che il mercato &egrave; al ribasso. Al contrario, se la Chikou Span supera il prezzo di mercato, indica che il mercato &egrave; al rialzo.</p>")

# MACD ----
HTML_Scripts[["MACD"]]<-paste0("<p>Il <a href='https://www.ig.com/it/strategie-di-trading/come-fare-trading-con-lindicatore-macd--moving-average-convergen-201030'>Moving Average Convergence Divergence (MACD)</a> &egrave; un indicatore che fornisce informazioni sia sulla direzione della tendenza, al rialzo (rialzista) o al ribasso (ribassista), sia sulla forza del trend.</p>
                                 
                                 <p><b>Calcolo dell'indicatore MACD</b><br />
Il calcolo dell'indicatore MACD si basa sulle medie mobili esponenziali, in inglese Exponential Moving Averages (EMA), in cui i dati pi&ugrave; recenti contano di pi&ugrave; rispetto a una media ordinaria. Di conseguenza, gli sviluppi recenti del trading influenzano maggiormente la media.</p>
                                 
                                 <p><b>Linea MACD, linea del segnale e istogramma</b><br />
                                 L'indicatore MACD &egrave; composto da due linee. La prima &egrave; la linea MACD. Questa linea viene creata calcolando la differenza tra la media mobile esponenziale su un periodo pi&ugrave; lungo e quella su un periodo pi&ugrave; breve. La seconda linea &egrave; la cosiddetta linea di segnale. Questa linea fornisce la media mobile esponenziale della linea MACD stessa. Il calcolo della differenza tra la linea MACD e la linea del segnale crea un istogramma, o grafico a colonne.</p>

<p><b>Periodo della linea media del MACD e linea del segnale</b><br />
Una scelta importante per il calcolo dell'indicatore &egrave; il periodo (time frame) delle medie. Originariamente, per la media a lungo termine viene preso un periodo di ventisei giorni e per la media a breve termine un periodo di 12 giorni. &Egrave; possibile utilizzare anche periodi pi&ugrave; lunghi o pi&ugrave; brevi. Pi&ugrave; lungo &egrave; il periodo, pi&ugrave; gradualmente si muover&agrave; una media. Per la linea del segnale, gli investitori utilizzano solitamente nove giorni.</p>
                                 
                                 <p><b>Incrocio tra linea MACD e linea del segnale</b><br />
                                 Esistono diversi modi per leggere i segnali del MACD. Molti investitori guardano alla differenza tra la linea MACD e la linea del segnale e all'istogramma che mostra questa differenza. Se la linea MACD sale sopra la linea del segnale, l'istogramma sale sopra lo zero. Si tratta di un segnale di acquisto. Se la linea MACD scende sotto la linea del segnale, l'istogramma scende sotto lo zero. Questo &egrave; un segnale di vendita. In entrambi i casi, le due linee si incrociano. Per questo motivo si parla anche di &lsquo;&rsquo;crossover&rsquo;&rsquo;.</p>

<p><b>Linea MACD relativa alla linea dello zero</b><br />
&Egrave; anche possibile osservare la linea MACD stessa. In questo caso, gli investitori osservano l'andamento della linea MACD rispetto alla linea dello zero. Se la linea MACD &egrave; superiore a zero, il prezzo si trova in una tendenza al rialzo. Se la linea &egrave; inferiore a zero, il prezzo &egrave; in una tendenza al ribasso. Quanto la linea &egrave; al di sopra o al di sotto dello zero indica la forza della tendenza. Alcuni investitori vedono in una rapida variazione della linea MACD un segnale che indica che la tendenza &egrave; andata troppo veloce e che uno strumento finanziario &egrave; ipercomprato o ipervenduto. L'idea &egrave; che una tendenza positiva si stia rafforzando e che quindi sia ipervenduta. In questo caso, c'&egrave; spazio per una correzione e un'inversione di tendenza.</p>

<p><b>Divergenza e convergenza linea MACD e prezzo</b><br />
&Egrave; anche possibile ottenere segnali dall'andamento dei top e dei bottom dell'indicatore MACD rispetto ai top e ai bottom del grafico dei prezzi. Una tendenza positiva del prezzo pu&ograve; essere riconosciuta dal raggiungimento di un top sempre pi&ugrave; alto. In una tendenza negativa, il prezzo crolla e raggiunge fondi sempre pi&ugrave; bassi. Se la linea MACD mostra lo stesso andamento, si parla di convergenza tra la linea MACD e il prezzo. &Egrave; anche possibile che il prezzo sia in una tendenza al rialzo con top sempre pi&ugrave; alti, mentre i top nel grafico della linea MACD si stanno abbassando. In questo caso, c'&egrave; una divergenza tra il prezzo e la linea MACD. Alcuni investitori vedono la divergenza come un segnale di vendita.</p>")


# Le bande di Bollinger ----
HTML_Scripts[["BBollinger"]]<-paste0("<p>Le <a href='https://www.ig.com/it-ch/strategie-di-trading/cosa-sono-le-bande-di-bollinger-e-come-usarle-nel-trading-190122'>bande di Bollinger</a> sono formate da tre linee distinte sul grafico che indicano delle fasce di prezzo. La linea centrale rappresenta la media mobile del prezzo (di default si usa la media mobile semplice a 20 periodi). La banda superiore &egrave; calcolata aggiungendo una deviazione standard del prezzo al valore della media mobile. La banda inferiore &egrave; calcolata sottraendo una deviazione standard del prezzo al valore della media mobile.</p>

<p>Come funzionano<br>
Le bande superiore e inferiore formano un range di prezzo (le envelopes) che racchiudono, nella maggior parte dei casi, i movimenti del mercato (valori massimi e minimi). Le bande di Bollinger superiore e inferiore misurano cos&igrave; quella che &egrave; la dispersione del prezzo intorno alla media mobile e la volatilit&agrave;. Pi&ugrave; le bande sono vicine o strette tra loro, pi&ugrave; bassa sar&agrave; la volatilit&agrave; dei prezzi. Per gli analisti tecnici la contrazione delle bande rappresenta un periodo di consolidamento o di assestamento del mercato. Pi&ugrave; le bande sono lontane fra loro, maggiore sar&agrave; la volatilit&agrave; dei prezzi. In fase di espansione, le bande indicano spesso l&rsquo;inizio di un nuovo trend di prezzo.</p>

<p>Punti di rottura (breakout) e contrazioni (squeeze) delle bande<br>
Come gi&agrave; menzionato sopra, le bande di Bollinger identificano un mercato in fase di consolidamento nei periodi di bassa volatilit&agrave;. I breakout trader analizzano queste fasi, aspettando che una nuova opportunit&agrave; di trading emerga alla fine dei periodi di consolidamento. Quando le bande sono contratte, la banda superiore spesso rappresenta una resistenza, mentre la banda inferiore rappresenta un supporto. Qualora il prezzo dovesse superare la resistenza o scendere sotto il supporto, potrebbero nascere nuove opportunit&agrave; di trading per gli investitori.<br>
Sul mercato potrebbe partire cos&igrave; un nuovo trend di prezzo. I trader, quindi, possono sfruttare questa opportunit&agrave; di entrata per fare trading seguendo il nuovo andamento dei prezzi. La rottura delle bande di Bollinger &egrave; interpretata, quindi, come segnale di trading vero e proprio. &Egrave; possibile notare, in casi simili, una netta espansione delle bande di Bollinger. L&rsquo;espansione delle bande segnala anche che il mercato sta uscendo dalla fase di consolidamento e che il prezzo si sta muovendo seguendo una direzione diversa.<br>
Le bande di Bollinger non riescono a fornire suggerimenti esatti sulle strategie di trading da adottare, tuttavia il segnale di uscita si ha quando le bande di Bollinger entrano in fase di espansione oppure quando ricominciano a contrarsi (andamento del mercato al rialzo o al ribasso). La strategia di trading sui punti di rottura, definita Bollinger breakout, &egrave; anche nota come &ldquo;Bollinger Squeeze&rdquo;, o contrazione di Bollinger, poich&eacute; le bande &ldquo;schiacciano&rdquo; i prezzi prima di arrivare al punto di rottura.</p>

<p>Le bande di Bollinger nell&rsquo;inversione del trend<br>
Oltre al trading con i breakout, le bande di Bollinger sono utilizzate per identificare le inversioni del trend dei prezzi in due forme: il doppio massimo (&ldquo;M&rdquo;) e il doppio minimo (&ldquo;W&rdquo;).<br>
L&rsquo;inversione a &quot;M&rdquo; della teoria di Bollinger si compone di due picchi massimi di prezzo (punti di svolta) su un trend di mercato rialzista. Il primo punto di svolta supera la banda di Bollinger superiore, mentre il secondo punto di svolta si trova leggermente sotto il limite della banda superiore. Il mancato superamento del secondo punto della banda superiore suggerisce che il rialzo del prezzo &egrave; pi&ugrave; debole rispetto a quello raggiunto dal punto massimo precedente. In questa fase il mercato &egrave; nella zona di ipercomprato, per cui il trend rialzista si troverebbe in fase conclusiva e potrebbe manifestarsi un&rsquo;inversione ribassista. I trader che hanno aperto posizioni long sul mercato possono, in questo caso, decidere di incassare i profitti oppure chiudere le posizioni. I trader che puntano sull&rsquo;inversione del pattern di prezzo attendono un&rsquo;altra rottura del successivo trend come segnale di conferma, prima di aprire posizioni short sul mercato scelto.<br>
L&rsquo;inversione a &ldquo;W&rdquo; della teoria di Bollinger indica due livelli minimi di prezzo (punti di svolta) su un trend ribassista del mercato. Il primo punto di svolta supera la banda inferiore di Bollinger, mentre il secondo punto di svolta si trova leggermente al di sopra della banda inferiore. Il mancato superamento del secondo punto minimo della banda inferiore ci suggerisce che il movimento del prezzo &egrave; pi&ugrave; debole rispetto a quello raggiunto dal punto minimo precedente. In questa fase il mercato &egrave; in fase di ipervenduto, per cui il trend ribassista si troverebbe in fase conclusiva e potrebbe manifestarsi un&rsquo;inversione rialzista. I trader che hanno aperto posizioni short possono, in questo caso, incassare i profitti oppure chiudere le posizioni. I trader che puntano sull&rsquo;inversione del pattern di prezzo attendono una rottura del successivo trend come segnale di conferma, prima di aprire posizioni long sul mercato scelto.</p>")


# L'oscillatore stocastico ----
HTML_Scripts[["StochasticOscillator"]]<-paste0("<p><a href='https://www.ig.com/it/strategie-di-trading/guida-completa-al-trading-con-loscillatore-stocastico-210105'>L&rsquo;Oscillatore Stocastico</a> valuta quanto le chiusure delle barre siano prossime al massimo o al minimo registrati in un certo periodo (di default nelle pi&ugrave; diffuse piattaforme di trading troviamo un periodo di 14 barre). L&rsquo;intuizione di Lane consiste nell&rsquo;aver compreso che, di norma, le chiusure di barra prossime al loro massimo sono tipiche di tendenze al rialzo, mentre chiusure di barra verso il minimo sono indice di un trend ribassista.</p>

<p>I diversi tipi di stocastico<br>
Esistono ben tre versioni di stocastico. Lo stocastico veloce (fast stochastic) rappresenta la formula originale di Lane nel quale la linea %K si ottiene mettendo in relazione percentuale il prezzo di chiusura ed un certo range di prezzo (a 14 periodi), mentre la linea %D &egrave; semplicemente la media mobile (normalmente a 3 periodi) della linea %K.<br>
Tuttavia lo stocastico c.d. veloce ha il difetto di essere troppo reattivo e di difficile applicazione. Per ovviare a questo problema si &egrave; creata una variante che genera segnali meglio interpretabili e che viene definita come stocastico lento (low stochastic). In questa versione dello stocastico la linea %K &egrave; costruita come media mobile (a 3 periodi) della linea %D della versione dello stocastico veloce, mentre la linea %D rappresenta a sua volta la media mobile (a 3 periodi) della nuova linea %K.<br>
Vi &egrave; poi una versione che viene definita stocastico completo (full stochastic) che &egrave; una variante personalizzabile dello stocastico lento. In questa ultima versione possiamo impostare tutti i parametri necessari, il numero di periodi da prendere in considerazione, il numero di periodi per il %K e il numero di periodi del %D. Quest&rsquo;ultima versione &egrave; quella pi&ugrave; utilizzata dalla maggior parte dei trader.</p>

<p>Come si interpreta<br>
Lo stocastico &egrave; un oscillatore e pertanto si muove o &lsquo;oscilla&rsquo; fra i valori 0 e 100. Non importa quanto il prezzo del nostro titolo si muover&agrave; al ribasso o al rialzo, lo stocastico fluttuer&agrave; sempre fra i valori 0 e 100. Uno degli utilizzi pi&ugrave; elementari dello stocastico &egrave; quello di individuare zone di eccesso dei prezzi, ovvero il c.d. ipervenduto quando abbiamo valori sotto il valore 20 e il c.d. ipercomprato quando questi valori superano il valore 80. Ovviamente possiamo variare a piacimento questi valori per adattarli allo strumento finanziario sul quale stiamo operando. &Egrave; corretto sottolineare che qualora i prezzi si trovino nelle zone di ipercomprato e ipervenduto non dobbiamo attenderci necessariamente una inversione di tendenza, o quantomeno non necessariamente l&rsquo;inversione sar&agrave; immediata. Spesso, infatti, lo stocastico entra in area di ipercomprato e ipervenduto in situazioni di trend sostenuto e ben definito e l&rsquo;oscillatore si mantiene in tali zone a lungo senza mai varcare la linea di demarcazione 80-20. Motivo per il quale &egrave; bene porre la massima attenzione nel non utilizzare questo oscillatore per catturare automaticamente segnali contro trend.<br>
Al contrario, questo oscillatore &egrave; eccezionale per generare segnali durante le fasi laterali, di non-trend.</p>

<p>Utilizzo in un mercato laterale (o di trend non ben definito)<br>
VEDI ARTICOLO</p>

<p>Utilizzo dell&rsquo;oscillatore stocastico in un mercato in tendenza<br>
L&rsquo;oscillatore Stocastico &egrave; molto efficace e in grado di generare ottimi segnali di trading non solo nelle fasi laterali propriamente dette, laddove cio&egrave; i prezzi si muovono da un supporto ad una resistenza statici, ma anche nelle fasi di moderata tendenza dei prezzi. Risulta invece piuttosto rischioso utilizzare lo stocastico per operare contro trend, ovvero quando lo strumento considerato mostra una buona tendenza, rialzista o ribassista. Ne consegue che, se si vuole utilizzare questo oscillatore nelle fasi di trend, occorre attendere lo sviluppo di una tendenza ben definita, quindi un successivo ritracciamento e sfruttare lo stocastico per entrare in direzione del trend originario, cercando di catturare la fine della correzione. Un consiglio operativo per ridurre il numero di falsi segnali &egrave; quello di settare il nostro oscillatore utilizzando i parametri 20-5-5 al posto di quelli 14-3-3 proposti di default dalle piattaforme di analisi grafica. Naturalmente anche in questo caso, il consiglio resta quello di fare dei test approfonditi per valutare quali, nel passato, siano stati i migliori settaggi dell&rsquo;indicatore in grado di restituire il minor numero possibile di falsi segnali.</p>

<p>Operativit&agrave; in divergenza<br>
Un altro tipo di operativit&agrave; con lo stocastico prevede l&rsquo;individuazione di divergenze fra prezzi e oscillatore come filtro per aprire la posizione. Le divergenze sull&rsquo;oscillatore si verificano abbastanza spesso e sono in genere piuttosto affidabili se associate ad altri segnali di trading. &Egrave; preferibile utilizzare le divergenze su movimenti con una moderata tendenza, ancor meglio, in assenza di tendenza (fasi laterali) in quanto le probabilit&agrave; di successo dell&rsquo;operazione sono pi&ugrave; elevate. Mentre con titoli in forte trend potremmo assistere a divergenze multiple prima che si abbia una effettiva inversione di trend, con diversi inevitabili falsi segnali. Anche per questo motivo &egrave; consigliabile utilizzare le divergenze non come vero e proprio segnale di ingresso in posizione, ma come filtro di conferma da associarsi ad altre tecniche che sfruttino direttamente le relazioni tra i prezzi.</p>")




# Yang Zhang Volatility ----
HTML_Scripts[["YZ_Volatility"]]<-paste0("<p>Lo stimatore della volatilit&agrave; storica di Yang e Zhang ha un errore di stima minimo ed &egrave; indipendente dalla deriva e dai gap di apertura. Pu&ograve; essere interpretato come una media ponderata dello stimatore di Rogers e Satchell, della volatilit&agrave; di chiusura-apertura e della volatilit&agrave; di apertura-chiusura. Gli utenti possono ignorare i valori predefiniti di &#945; (1,34 per impostazione predefinita) o k utilizzati nel calcolo specificando rispettivamente alpha o k in .... Specificando k, alpha verr&agrave; ignorato se entrambi vengono forniti.</p>

<p><b>Calcolo</b><br>
La volatilit&agrave; di Yang-Zhang combina tre componenti principali:</p>

<p> 1. Volatilit&agrave; overnight (close-to-open): Misura la variazione di prezzo tra la chiusura di un giorno e l'apertura del giorno successivo.</p>

<p> 2. Volatilit&agrave; open-to-close: Misura la variazione di prezzo durante il giorno di trading.</p>

<p> 3. Volatilit&agrave; Rogers-Satchell: Una media ponderata che tiene conto delle variazioni di prezzo tra apertura, chiusura, massimo e minimo.</p>

<p>La formula di Yang-Zhang &egrave; progettata per gestire sia i salti di apertura che il drift, riducendo al minimo l'errore di stima.</p>

<p><b>Vantaggi</b></p>

<p> * Efficienza: &Egrave; quattordici volte pi&ugrave; efficiente rispetto al metodo close-to-close.</p>

<p> * Precisione: Gestisce i salti di apertura e il drift, offrendo una stima pi&ugrave; accurata della volatilit&agrave;.</p>

<p> * Minimo errore di stima: Riduce al minimo l'errore di stima rispetto ad altri metodi.</p>

<p>La volatilit&agrave; misura l'intensit&agrave; delle variazioni di prezzo di un asset finanziario in un determinato periodo di tempo. Ecco una panoramica dei valori alti e bassi di volatilit&agrave;:</p>

<p><b>Valori Bassi di Volatilit&agrave;</b></p>

<p> * Stabilit&agrave;: Il valore dell'asset non fluttua drasticamente e tende ad essere pi&ugrave; stabile.</p>

<p> * Rischio Minore: Gli investitori sono esposti a un rischio minore, ma anche le opportunit&agrave; di guadagno sono pi&ugrave; limitate.</p>

<p><b>Valori Alti di Volatilit&agrave;</b></p>

<p> * Instabilit&agrave;: Il valore dell'asset pu&ograve; cambiare drasticamente in un breve lasso di tempo, sia al rialzo che al ribasso.</p>

<p> * Rischio Maggiore: Gli investitori sono esposti a un rischio maggiore, ma anche le opportunit&agrave; di guadagno possono essere pi&ugrave; elevate.</p>

<p>In generale, un'alta volatilit&agrave; implica che il prezzo di un asset pu&ograve; variare ampiamente in un breve periodo di tempo, mentre una bassa volatilit&agrave; indica che il prezzo tende a cambiare in modo pi&ugrave; graduale. La volatilit&agrave; &egrave; solitamente espressa in termini percentuali. Ecco una guida generale per interpretare i valori di volatilit&agrave;:</p>

<p><b>Valori Bassi di Volatilit&agrave;</b></p>

<p> * 0% - 10%: Molto bassa. Tipica di titoli di stato o obbligazioni di alta qualit&agrave;.</p>

<p> * 10% - 20%: Bassa. Tipica di grandi aziende stabili con fluttuazioni di prezzo limitate.</p>

<p><b>Valori Alti di Volatilit&agrave;</b></p>

<p> * 20% - 40%: Alta. Tipica di azioni di societ&agrave; tecnologiche o settori in rapida crescita.</p>

<p> * Oltre 40%: Molto alta. Tipica di criptovalute o titoli altamente speculativi</p>")



