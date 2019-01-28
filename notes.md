
* div
	* button
	  - text: "hello world"
	  - onclick: (asd)
	  <shadow> (or not)

prefabs
* button
* modals/popups placeholder

* cont1
 * placeholder1
 * placeholder1
 * [placeholders]


dom-element
	tag: div
	attrs: -
	styles: -

a: dom-element
	tag: a
	attrs: href: -



- render dom elem
- loader layer
- fetch from db - graphql -> fields
- in-modal?
- dragable
- dropable
- timer
- db-error-handler
- form-error-handler?
- GA event
- GA context

<a>asdf 1 </a>

assets:
- imges1
- fonts
- icons
- prefabs



- latwa customizacja gotowych prefabs (merge map z konfiguracja)
- dobre deafulty z mozliwoscia customizacji
- (sync prefabs?)
- wsparcie dla js/cljs/... (typescript, inne) w "skryptach"

- https://dev-blog.apollodata.com
- assets store
- rozszerzenia do edytora




- mixinx
 - events-handler
 - local-state
 -

- "wskazniki" na wartości w store
- store podzielony na external i internal(?) (z historia)
- grqphql do stanu

[(get-in) .. .. .. ..]
[nazwa-komponentu mixin? pole]
- pola komponentow/mixinów (tylko localstate?) otypowane
- sledzić "wskazniki"
- parent(this, typ), cmp interface w graphql

- kazdy definiue inputy (z globalnych albo lokalne (local-state lub parametry)
- dla kazdego komponentu liczyć wchodzace dane (internal-props: external-reference)

- prefab / sync (mapa nadpisująca, można zacomitować do wzorca)

- focus na komponent przy edytowaniu
- graf zaleznosci danych, wyliczne dane
- infer zaleznosci od danych i wysylanych eventow





PARTS
- name
- props, with meta data
- livecycle fns
- outputs/public local state, querable

PART invocation
{:name {:prop1 A, :prop2 Y}}


COMPONENT
- name
- parts
- 


- paths, github.com/nathanmarz/specter, or get-in?


- wymienialne moduły?
- namespaces qualifided names