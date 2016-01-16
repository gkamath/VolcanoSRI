from pylab import *
#from numpy import linalg as LA
def topology(string,n):
    disconnect = 0
    if(string == 'RGG'):
        pos = rand(n,2)
        a = matrix(pos[:,0]).transpose()
        atrans = (matrix(pos[:,0]))
        b = matrix(pos[:,1]).transpose()
        btrans = (matrix(pos[:,1]))
        dist = sqrt(square(matrix((kron(ones((1,n)),a) - kron(ones((n,1)),atrans)))) + square(kron(ones((1,n)),b) - kron(ones((n,1)),btrans)))
        thresh = sqrt(2*math.log(n)/n)      
        G = (dist <=thresh - identity(n))   
        if(((extract(G.sum(axis=0)!=0,G.sum(axis=0))).size) != (array((G.sum(axis=0))).size)):
            disconnect = 1     
        while(disconnect == 1):
            pos = rand(n,2)
            a = matrix(pos[:,0]).transpose()
            atrans = (matrix(pos[:,0]))
            b = matrix(pos[:,1]).transpose()
            btrans = (matrix(pos[:,1]))
            dist = sqrt(square(matrix((kron(ones((1,n)),a) - kron(ones((n,1)),atrans)))) + square(kron(ones((1,n)),b) - kron(ones((n,1)),btrans)))
            thresh = sqrt(2*math.log(n)/n)        
            G = (dist <=thresh - identity(n))
            #G[:,1] = 0
        
            if(((extract(G.sum(axis=0)!=0,G.sum(axis=0))).size) == (array((G.sum(axis=0))).size)):
                disconnect = 0
                
    if(string == 'Kn'):
        pos = rand(n,2) 
        G = ones((n,n)) - identity(n)
    
    if(string == 'Chain'):
        pos=zeros((n,2))
        G=zeros((n,n))
        G[0,1] = 1
        G[n-1,n-2] = 1
        for i in range(1,n-1):
            G[i,i+1]=1
            G[i,i-1]=1
        pos[:,0]=linspace(0,1,n)
        pos[:,1]=0.5*ones((1,n))
    if(string == 'Ring'):
        pos=zeros((n,2))
        G=zeros((n,n))
        G[0,1] = 1
        G[n-1,n-2] = 1
        for i in range(1,n-1):
            G[i,i+1]=1
            G[i,i-1]=1
	G[0,n-1] = 1
	G[n-1,0] = 1
        pos[:,0]=linspace(0,1,n)
        pos[:,1]=0.5*ones((1,n))    
    if(string=='Star'):
        pos=rand(n,2)
        G = zeros((n,n))
        for i in range(0,n-1):
            G[0,i]=1
            G[i,0]=1
    if(string=='Grid'):
        m=int(sqrt(n))
        pos =zeros((n,2))
        for i in range(0,n-1):
            if(mod(i,m) == 0):
                pos[i,0]=1
                pos[i,1]=(floor(i/m)-1)/(m-1)
            else:
                pos[i,0]=(mod(i,m)-1)/(m-1)
                pos[i,1]=(floor(i/m))/(m-1)
        G=zeros((n,n))      
        GTemp = zeros((n+1, n+1))
        GTemp[1,m+1]=1    
        for i in range(2,m):
            GTemp[i,i-1]=1
            GTemp[i-1,i]=1
            GTemp[i,i+1]=1
            GTemp[i+1,i]=1
        for i in range((n-m+2), n):
            GTemp[i,i-1]=1
            GTemp[i-1,i]=1
            GTemp[i,i+1]=1
            GTemp[i+1,i]=1
        for i in range(m+1,n-m+1):
            if( mod(i,m)==0):
                GTemp[i,i-m]=1
                GTemp[i-m,i]=1
                GTemp[i,i+m]=1
                GTemp[i+m,i]=1
            if( mod(i,m)==1):
                GTemp[i,i-m]=1
                GTemp[i-m,i]=1
                GTemp[i,i+m]=1
                GTemp[i+m,i]=1
        for i in range((m+2),(n-m)):
            if( mod(i,m)!=0 and mod(i,m)!=1):
                GTemp[i,i-1]=1
                GTemp[i-1,i]=1
                GTemp[i,i+1]=1
                GTemp[i+1,i]=1
                GTemp[i,i-m]=1
                GTemp[i-m,i]=1
                GTemp[i,i+m]=1
                GTemp[i+m,i]=1
        G = GTemp[1:,1:]
    return G
        #savetxt('text.txt',G,fmt='%.2f')
