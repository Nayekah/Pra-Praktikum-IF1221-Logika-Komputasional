/*Untuk File .pl*/


/* Bagian I */
/* Deklarasi Fakta */

pria(qika).
pria(panji).
pria(shelby).
pria(barok).
pria(aqua).
pria(eriq).
pria(francesco).

wanita(hinatsuru).
wanita(makio).
wanita(suma).
wanita(frieren).
wanita(yennefer).
wanita(roxy).
wanita(ruby).
wanita(suzy).
wanita(aihoshino).
wanita(eve).

usia(hinatsuru, 105).
usia(qika, 109).
usia(makio, 96).
usia(suma, 86).
usia(panji, 124).
usia(frieren, 90).
usia(shelby, 42).
usia(yennefer, 61).
usia(barok, 59).
usia(roxy, 70).
usia(aqua, 66).
usia(ruby, 63).
usia(eriq, 69).
usia(suzy, 23).
usia(francesco, 25).
usia(aihoshino, 48).
usia(eve, 5).

menikah(qika, hinatsuru).
menikah(qika, makio).
menikah(qika, suma).
menikah(hinatsuru, qika).
menikah(makio, qika).
menikah(suma, qika).
menikah(panji, frieren).
menikah(frieren, panji).
menikah(barok, roxy).
menikah(roxy, barok).
menikah(eriq, ruby).
menikah(ruby, eriq).
menikah(francesco, suzy).
menikah(suzy, francesco).

anak(shelby, qika).
anak(shelby, hinatsuru).
anak(yennefer, qika).
anak(yennefer, hinatsuru).
anak(barok, qika).
anak(barok, makio).
anak(aqua, panji).
anak(aqua, frieren).
anak(ruby, panji).
anak(ruby, frieren).
anak(suzy, barok).
anak(suzy, roxy).
anak(aihoshino, eriq).
anak(aihoshino, ruby).
anak(eve, francesco).
anak(eve, suzy).


/* Deklarasi Rules */
saudara(X, Y) :- anak(X, Z), anak(Y, Z), anak(X, A), anak(Y, A), pria(Z), wanita(A), X \== Y.
saudara(X, Y) :- saudaratiri(X,Y).

saudaratiri(X, Y) :- anak(X, Z), anak(Y, Z), anak(X, A), anak(Y, B), pria(Z), wanita(A), wanita(B), A\==B, X \== Y.
saudaratiri(X, Y) :- anak(X, Z), anak(Y, Z), anak(X, A), anak(Y, B), wanita(Z), pria(A), pria(B), A\==B, X \== Y.

kakak(X, Y) :- saudara(X, Y), usia(X, UsiaX), usia(Y, UsiaY), UsiaX > UsiaY.

keponakan(X, Y) :- anak(X, Z), saudara(Z, Y), \+ saudaratiri(Z,Y).

mertua(X,Y) :- menikah(Y, Z), anak(Z, X).

nenek(X,Y) :- wanita(X), anak(Z, X), anak(Y, Z).

keturunan(X,Y) :- anak(X, Y).
keturunan(X,Y) :- anak(X, Z), keturunan(Z, Y).

lajang(X) :- pria(X), \+menikah(X, Y), \+menikah(Y,X).
lajang(X) :- wanita(X), \+menikah(Y, X), \+menikah(X,Y).

anakbungsu(X) :- anak(X, A), pria(A), anak(Y, A), X \= Y, \+ saudaratiri(X, Y), \+ (anak(Z, A), X \= Z, \+ saudaratiri(X, Z), usia(Z, UsiaZ), usia(X, UsiaX), UsiaZ < UsiaX).

anaksulung(X) :- anak(X, A), pria(A), anak(Y, A), X \= Y, \+ saudaratiri(X, Y), \+ (anak(Z, A), X \= Z, \+ saudaratiri(X, Z), usia(Z, UsiaZ), usia(X, UsiaX), UsiaZ > UsiaX).

yatimpiatu(X) :- pria(X), \+anak(X, _Y).
yatimpiatu(X) :- wanita(X), \+anak(X, _Y).


/* Bagian II */
/* Deklarasi Rules */

/*Utils (Aturan Tambahan) */
rem(X,Y) :- 0 is X mod Y, !.
rem(X,Y) :- X > Y+1, rem(X, Y+1).

isPrime(2) :- !.
isPrime(X) :- X < 2, !, fail.
isPrime(X) :- \+rem(X, 2).

modTiga(TreeNumber, Fruits, Res) :- TreeNumber mod 3 =:= 0, Res is Fruits + 2.
modTiga(TreeNumber, Fruits, Fruits) :- TreeNumber mod 3 =\= 0.

modEmpat(TreeNumber, Fruits, Res) :- TreeNumber mod 4 =:= 0, Res is Fruits - 5.
modEmpat(TreeNumber, Fruits, Fruits) :- TreeNumber mod 4 =\= 0.

modLima(TreeNumber, Fruits, Res) :- TreeNumber mod 5 =:= 0, Res is Fruits + 3.
modLima(TreeNumber, Fruits, Fruits) :- TreeNumber mod 5 =\= 0.

modPrime(TreeNumber, Fruits, Res) :- isPrime(TreeNumber), Res is Fruits - 10.
modPrime(TreeNumber, Fruits, Fruits) :- \+ isPrime(TreeNumber).

fpb(A, 0, A).
fpb(0, A, A).
fpb(A, B, X) :- Rem is A mod B, fpb(B, Rem, X).

printPattern(_, Col, N) :- Col > N, !.
printPattern(Row, Col, N) :- pattern(Row, Col, N, Pattern), write(Pattern), write(' '), Next is Col + 1, printPattern(Row, Next, N).

pattern(Row, Col, N, Row) :- Row =< N/2, Row =< Col, Row =< N-Col+1, !.
pattern(Row, Col, N, Col) :- Col =< N/2, Col =< Row, Col =< N-Row+1, !.
pattern(Row, Col, N, Bottom) :- Bottom is N - Row + 1, Bottom =< Row, Bottom =< Col, Bottom =< N - Col + 1, !.
pattern(Row, Col, N, Right) :- Right is N - Col + 1, Right =< Row, Right =< Col, Right =< N - Row + 1.

/* Solver */

/* Exponent */
exponent(A,0,1) :- A \= 0, !.
exponent(A,B,X) :- B > 0, Inc is B - 1, exponent(A, Inc, Res), X is A * Res, ! .
exponent(A,B,X) :- B < 0, Ar is 1 / A, Br is B * -1, exponent(Ar, Br, X), ! .

/* Growth */
growth(I, _, _, 0, I).
growth(I, G, H, T, X) :- T > 0, Trec is T - 1, growth(I, G, H, Trec, Px), isPrime(T), X is Px + G.
growth(I, G, H, T, X) :- T > 0, Trec is T - 1, growth(I, G, H, Trec, Px), \+ isPrime(T), X is Px - H.

/* Si Imut Anak Nakal */
harvestFruits(N, Fruits, TreeNumber, Fruits) :- TreeNumber > N.
harvestFruits(_, Fruits, _, 0) :- Fruits =< 0, write('Si Imut pulang sambil menangis :(').
harvestFruits(N, Fruits, TreeNumber, FinalResults) :-
    TreeNumber =< N,
    modTiga(TreeNumber, Fruits, Res1),
    modEmpat(TreeNumber, Res1, Res2),
    modLima(TreeNumber, Res2, Res3),
    modPrime(TreeNumber, Res3, Res),
    Next is TreeNumber + 1,
    harvestFruits(N, Res, Next, FinalResults).

/* KPK */
kpk(A, B, X) :- fpb(A, B, Gcd), X is (A * B) // Gcd.

/* Factorial */
factorial(0, 1).
factorial(N, X) :- N > 0, Inc is N - 1, factorial(Inc, Fac), X is N * Fac.

/* Make Pattern */
/* Cara pemanggilan sama */
makePattern(0, _) :- !.
makePattern(Row, N) :- printPattern(Row, 1, N), nl, Next is Row - 1, makePattern(Next, N).
makePattern(N) :- N >= 0, makePattern(N, N).


/* Bagian III */
/* Deklarasi Rules */

/* Utils (Aturan Tambahan) */
insertLast([], X, X) :- !.
insertLast([A|B], C, [A|D]) :- insertLast(B, C, D).

combineList([], L, L).
combineList([Head|T], L, Res) :- combineList(T, L, Temp), insertLast([Head], Temp, Res).

splitList(List, N, Fi, Se) :- splitL(List, N, [], Fi, Se).

splitL(Rest, 0, L, Fi, Rest) :- reverselist(L, Fi).
splitL([Head|T], N, L, Fi, Se) :- N > 0, N1 is N - 1, splitL(T, N1, [Head|L], Fi, Se).

lst([X], X).
lst([_|T], X) :- lst(T, X).

dellst([_], []).
dellst([Head|T], [Head|R]) :- dellst(T, R).

rotateLeft(List, 0, List).
rotateLeft(List, N, Result) :- N > 0, splitList(List, N, Fi, Se), combineList(Se, Fi, Result).

mapn(Nilai, 'A') :- Nilai >= 80, !.
mapn(Nilai, 'B') :- Nilai >= 70, !.
mapn(Nilai, 'C') :- Nilai >= 60, !.
mapn(Nilai, 'D') :- Nilai >= 50, !.
mapn(Nilai, 'E') :- Nilai < 50, !.

avg(List, Avg) :- summ(List, Sum), countt(List, Count), Count > 0, Avg is Sum / Count.
lulus(Avg, 'Pass') :- Avg >= 80, !.
lulus(Avg, 'Fail') :- Avg < 80, !.

konvNilai([], []).
konvNilai([Head|T], [KHead|KT]) :- mapn(Head, KHead), konvNilai(T, KT).

/* Solver */

/* List Statistic */
/* Pemanggilan sama, hanya ditambahkan postfix di akhir untuk memastikan tidak menggunakan fungsi bawaan prolog */
mini([X], X).
mini([Head|T], Min) :- mini(T, PrevMin), Min is Head, Head =< PrevMin.
mini([Head|T], Min) :- mini(T, PrevMin), Min is PrevMin, Head > PrevMin.

maxi([X], X).
maxi([Head|T], Max) :- maxi(T, PrevMax), Max is Head, Head >= PrevMax.
maxi([Head|T], Max) :- maxi(T, PrevMax), Max is PrevMax, Head < PrevMax.

rangee([], 0).
rangee([Head|T], Range) :- maxi([Head|T], Max), mini([Head|T], Min), Range is Max - Min.

countt([],0).
countt([_|T],Len) :- countt(T,Ct), Len is Ct + 1.

summ([], 0).
summ([Head|T], Sum) :- summ(T, Elmt), Sum is Head + Elmt.

/* mergeSort */
mergeSort([], Se, Se).
mergeSort(Fi, [], Fi).
mergeSort([Fi|TA], [Se|TB], [Se|Res]) :- Fi > Se, mergeSort([Fi|TA], TB, Res).
mergeSort([Fi|TA], [Se|TB], [Fi|Res]) :- Fi =< Se, mergeSort(TA, [Se|TB], Res).

/* filterArray */
filterArray([], _, _, []) :- !.
filterArray([Head|T], El1, El2, [Head|Res]) :- Head > El1, (Head mod El2) =:= 0, filterArray(T, El1, El2, Res), !.
filterArray([_|T], El1, El2, Res) :- filterArray(T, El1, El2, Res).

/* reverse */
/* Cara pemanggilan sama, hanya saja ditambahkan postfix untuk memastikan tidak menggunakan fungsi bawaan prolog */
reverselist(X, Y) :- reverselist(X, [], Y).
reverselist([], A, A).
reverselist([Head|T], X, Y) :- reverselist(T, [Head|X], Y).

/* cekPalindrom */

cekPalindrom([]).
cekPalindrom([_]).
cekPalindrom([Head|T]) :- lst(T, X), Head == X, dellst(T, Res), cekPalindrom(Res).

/* rotate */
rotate([], _, []).
rotate(List, N, Result) :- N < 0, countt(List, Len), Pos is Len + (N mod Len), rotate(List, Pos, Result).
rotate(List, N, Result) :- N > 0, countt(List, Len), Len > 0, Norm is (N mod Len), rotateLeft(List, Norm, Result).

/* Mapping */
prosesMahasiswa(Nama, Nilai, Res) :- konvNilai(Nilai, Cvt), avg(Nilai, Avg), lulus(Avg, Status), Res = [Nama, Cvt, Avg, Status].


/* Bagian IV (BONUS) */
/* Deklarasi Rules */


/* Solver */
/* Predikat dinamis */
:- dynamic(isStarted/1).
:- dynamic(coins/1).

/* Fakta Awal */
isStarted(tidak).
coins(0).

/* Start permainan */
start :- isStarted(iya), write('Permainan sudah dimulai. Gunakan "exit" untuk keluar dan memulai ulang.'), !.
start :- isStarted(tidak), retract(isStarted(tidak)), assertz(isStarted(iya)), retract(coins(_)), assertz(coins(0)),
    write('Unta putih dan unta hitammu diambil oleh pedagang karavan. Kumpulkan koin sebanyak-banyaknya !!!').

/* move (North ,South, East, West) */
move(_) :- isStarted(tidak), !, fail.

move(Direction) :-
    isStarted(iya),(Direction = north; Direction = south; Direction = east; Direction = west),
    format('Kamu bergerak ke arah ~w.~n', [Direction]),
    random(1, 5, Chance),
    (
        Chance = 3 -> 
            write('Oh tidak! Kamu terkena jebakan!'), nl,
            coins(Curr),
            New is Curr - 10,
            retract(coins(Curr)),
            assertz(coins(New))
;
            write('Jalannya aman.'), nl,
            write('Kamu mendapatkan 10 poin.'), nl,
            coins(Curr),
            New is Curr + 10,
            retract(coins(Curr)),
            assertz(coins(New))
    ),
    coins(Current),
    format('Koin Saat Ini: ~d~n', [Current]), !.

/* Status jumlah koin */
display_status :- isStarted(tidak), !, fail.
display_status :- isStarted(iya), coins(Curr), format('Koin Saat Ini: ~d~n', [Curr]), !.

/* Exit permainan */
exit :- isStarted(tidak), write('Permainan belum dimulai. Gunakan "start" untuk memulai.'), !, fail.
exit :- isStarted(iya),
    write('Terima kasih telah memainkan simulasi ini! Sampai jumpa lagi.'), retract(isStarted(iya)), assertz(isStarted(tidak)), !.