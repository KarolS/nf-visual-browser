package pl.umk.mat.stasiu88.nfserver.datasource

trait DevDataSourceComponent extends DataSourceComponent {
  val dataSource = new DirectoryDataSource("/media/DOKUMENTY/NF")
}