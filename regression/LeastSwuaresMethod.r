
library(tidyverse)

LSM <- function(df,X,Y){
    # 列名を文字列で指定してデータを取得
    x_data <- df[[X]]
    y_data <- df[[Y]]

    beta_1 <- cov(x_data,y_data)/var(x_data)
    beta_0 <- mean(y_data)-beta_1*mean(x_data)
    return(list(beta_0 = beta_0, beta_1 = beta_1))
}

result <- LSM(df,"x1","y1")
y_hat <- result$beta_0 + result$beta_1*x1
df$y_hat <- y_hat

# プロットの作成
g <- ggplot(df, aes(x = x1, y = y1)) +
  geom_point(color = "blue", size = 2) +          # 実測値のプロット
  geom_line(aes(y = y_hat), color = "red", linewidth = 1) +  # 予測値のプロット
  labs(title = "実測値と予測値の比較", x = "x1", y = "y1") +
  theme_minimal()

print(g)
