library('mctest')
head(Hald)
x <- Hald[ , -1]
y <- Hald[ , 1]
omcdiag (x, y, detr = 0.001, red = 0.6, conf = 0.99, theil = 0.6, cn = 15)
omcdiag (x, y, Inter = FALSE)
omcdiag (x, y)
omcdiag(x = x, y = y)


imcdiag(x, y, corr = TRUE)
imcdiag(x, y)


imcdiag(x, y, method = "VIF", vif = 5)
imcdiag(x, y, method = "VIF", vif = 10, corr = TRUE)
imcdiag(x, y, method = "CVIF", cvif = 10)


imcdiag(x, y, all = TRUE)
imcdiag(x, y, all = TRUE, vif = 15, conf = 0.99, )
imcdiag(x, y, method = "VIF", all = TRUE)
mctest(x, y, all = TRUE, type="i")


mc.plot(x, y)
mc.plot(x, y, vif = 10, ev = 0.1)
