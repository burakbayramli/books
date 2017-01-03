

begin macro yourmacro(h1, h2)
    this is just some text containing
    h1 and h2
end

begin macro hismacro(old, new)
tmp = old
old = new
new = tmp
end


yourmacro(one word, another word)

              hismacro(q,p)

a = 1
b = 2
# #macrofile ".macros.def"
c = 4
if test:
    mymacro(u[j], u[j-1], quadratic)  # macro!

i = 0  # boundary
for j in xrange(ny):
    update(T, Told, i, j, i+1, i-1, j+1, j+1)  # macro!
    
d = 9

trouble(Ti, p, q, r, p-1, q-1, r-1)

