### RDA
library(vegan)
library(ggplot2)
a <- read.csv("", header = T, fileEncoding = "GBK")
a <- read.csv("", header = T, row.names = 1,fileEncoding = "GBK")

names(a)
env <- a[,c(1:6)]
# em <- e[c(1:72),c(12:15)]
# env <- e[c(1:72),c(2:11)]
em <- a[,c(12:82)]
#em <- data.frame(t(em))
em <- decostand(em, "hellinger")
#em <- data.frame(scale(em))
# env <- read.csv("", header = T, fileEncoding = "GBK", row.names = 1)
# names(env)
# env <- env[,c(-11)]
res1 <- cca(em ~., env_demo, scale = TRUE)
summary(res1)
vif.cca(res1)
###
envfit <- envfit(res1,env,permutations  = 999)
envfit$vectors
r2 <- RsquareAdj(res1)
r2
spe_rda_adj <- r2$r.squared #  R2
spe_rda_exp_adj <- spe_rda_adj * res1$CCA$eig / sum(res1$CCA$eig)
spe_rda_exp_adj
# spe_rda_eig_adj <- spe_rda_exp_adj * spe.rda$tot.chi
RDA1_exp <- paste("RDA1:", round(spe_rda_exp_adj[1] * 100, 2), "%")
RDA2_exp <- paste("RDA2:", round(spe_rda_exp_adj[2] * 100, 2), "%")

spe_rda.scaling <- summary(res1, scaling = 3)
spe_rda.site <- data.frame(spe_rda.scaling$sites)[1:2]
spe_rda.site$name <- rownames(spe_rda.site)
#spe_rda.site$group <- e[c(73:120),]$Month
spe_rda.env <- data.frame(spe_rda.scaling$biplot)[1:2]
spe_rda.env$name <- rownames(spe_rda.env)
spe_rda.sp <- data.frame(spe_rda.scaling$species)[1:2]/3
spe_rda.sp$name <- rownames(spe_rda.sp)

library(ggrepel)
pp <- ggplot(spe_rda.site, aes(x = CCA1, y = CCA2)) +
  geom_point(size = 2, alpha = .6) +
  # geom_jitter(width = 0.2) +
  # stat_ellipse(aes(), level = 0.95, show.legend = FALSE, linetype = 1) + # 
  labs(x = RDA1_exp, y = RDA2_exp) + # 
  geom_hline(aes(yintercept = 0), colour = "gray", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), colour = "gray", linetype = "dashed") +
  # geom_segment(data = spe_rda.sp, aes(x = 0, y = 0, xend = CCA1 * 2, yend = CCA2 * 2), arrow = arrow(length = unit(0.2, "cm")), size = 0.8, color = "blue") +
  geom_segment(data = spe_rda.env, aes(x = 0, y = 0, xend = CCA1 * 2, yend = CCA2 * 2), arrow = arrow(length = unit(0.2, "cm")), size = 0.8, color = "black") +
  geom_text(data = spe_rda.env, aes(CCA1 * 2.4, CCA2 * 2.4, label = name), color = "black", size = 5) +
  # geom_text(data = spe_rda.sp, aes(RDA1 * 2.4, RDA2 * 2.4, label = name), color = "blue", size = 7) +
  # geom_text(data = spe_rda.site, aes(RDA1 * 2.4, RDA2 * 2.4, label = name), color = "gray", size = 5, alpha = .6) +
  # geom_text_repel(aes(RDA1 , RDA2,label = name), size = 4, alpha = 0.2) + #加样品名称
  # scale_x_continuous(limits = c(-1, 1)) +
  # scale_y_continuous(limits = c(-1, 1)) +
  theme(
    text = element_text(colour = "black", size = 19),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(size = 0.7, color = "black", fill = NA),
    legend.text = element_text(size = 16),
    legend.key = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_text(size = 16)
  )
pp
getwd()
library(eoffice)
topptx(filename = "CCA2.pptx", height = 5, width = 5)




library(rdacca.hp)
#env <- env[,c("SBD","SWC","AP","NH4.","P","pH","Fun_Chao1","INV","NRE","ACP")]
res <- rdacca.hp(em, env, method = "RDA", type = "adjR2", scale = T)
# 查看结果
res
# 绘图
plot(res)
res.Hier.part <- data.frame(res$Hier.part)
res.Hier.part$Variables <- rownames(res.Hier.part)
#res.Hier.part$Variables <- factor(res.Hier.part$Variables,
#                                 levels = c("FVC","CWM3","CWM7","CWM1","rainfall","CWM6","CWM2"))
contri <- ggplot(res.Hier.part, aes(Variables, I.perc...)) +
  geom_col(fill = "#1E90FF",width = 0.4) +
  labs(x = "", y = "Individual") +
  theme_bw() +
  coord_flip() +
  theme(
    text = element_text(colour = "black", size = 19),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # 设置X轴标签倾斜45度
    panel.border = element_rect(size = 0.7, color = "black", fill = NA)
  )
contri
topptx(filename = "", width = 4, height = 6)
getwd()
