nrp<-1000 # 繰り返し数
qi<-0.159 # qの初期値
n<-8 # 観測種子数
data<-matrix(c(4,3,4,5,5,2,3,1,4,0,1,5,5,6,5,4,4,5,3,4)) # 観測された生存種子数（＝データ）
logL<-numeric(nrp) # 行列logL初期化
q<-qi # 行列qの1つめに初期値を代入

# 以下繰り返し。値を更新しながら繰り返すのでapply系関数は使えないと思う
for (rp in 1:nrp){ 
  # q[rp]を用いて対数尤度の算出
  # 観測されたデータごとに二項分布を使って条件つき確率を計算。apply関数を使うことで繰り返し文を使わないようにしている
  lh<-apply(data,2,function(x){
    lhd<-choose(n,x)*q[rp]^x*(1-q[rp])^(n-x)
    return(lhd)
  })
  logL[rp]<-log(prod(lh)) # 確率の総乗が尤度、その対数をとって対数尤度
  
  # 繰り返しの2回目以降はqを変化させたときに対数尤度が高くなっていた場合のみ、そのqを採用
  if (rp>1){
    if (logL[rp]<logL[rp-1]){
      q[rp]<-q[rp-1]
      logL[rp]<-logL[rp-1]
    }
  }
  # 次のqはランダムに正負どちらかの隣を選ぶ
  if (round(runif(1))){ 
    q[rp+1]<-q[rp]+0.001
  }else{
    q[rp+1]<-q[rp]-0.001
  }
}
plot(q[1:nrp],type='l', main=q[nrp]) # 結果をグラフに