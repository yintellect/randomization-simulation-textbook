clear

mata

/* function insrow(m,v,r) */
/* inserts 1xp vector v as r-th row in kxp matrix m, */
/* moving down rows r..k in m into new rows (r+1)..(k+1). */
/* insrow() returns (k+1)xp matrix with v as r-th row */

function insrow(m,v,r) {
real matrix tmp
if (r==1) tmp=v\m
else if (r==rows(m)+1) tmp=m\v
else tmp=m[1..(r-1),.]\v\m[r..rows(m),.]
return(tmp)
}

end

mata

/* function perm(v) */
/* Permutes numerical mx1 vector v, */
/* returning mxm! matrix of permutations. */

function perm(v) {
real matrix tmp,ctmp,m,vnew
m=rows(v)
tmp=v[1]
for (j=2;j<=m;j++) {
ctmp=J(j,0,.)
for (k=1;k<=j;k++) {
vnew=J(1,cols(tmp),v[j])
ctmp=ctmp,insrow(tmp,vnew,k)
}
tmp=ctmp
}
real matrix result
result = uniqrows(tmp')'
st_matrix("permutation", result)
return(result)
}

end

