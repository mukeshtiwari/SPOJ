import re

if __name__ == "__main__":
    n = int ( raw_input() )
    c = 1
    while c <= n :
        email =  re.findall ( "[a-zA-Z0-9][a-zA-Z0-9._]{4,}@[a-zA-Z0-9]+\.(?:com|edu|org|co\.in)", raw_input() )  
        t = len ( email )
        print 'Case #' + str ( c ) + ': ' + str ( t )
        for i in xrange ( t ) : print email[i]
        c += 1




