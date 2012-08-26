/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing

import javax.swing._
import com.jgoodies.forms.layout.FormLayout
import com.jgoodies.forms.layout.CellConstraints
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.messages._
import org.jdesktop.swingx._
import org.joda.time._
import org.joda.time.format._
import scalaz._
import Scalaz._
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import pl.umk.mat.stasiu88.nfclient.swing.dialogs._
import javax.swing.tree.MutableTreeNode
import pl.umk.mat.stasiu88.nfclient.SubnetCache
import scala.collection.JavaConversions._

/**
 * A panel displayed in the "New Query" tab in the main window.
 * <br>
 * Panel wyświetlany wewnątrz zakładki "New Query" w oknie głównym.
 */
class NewQueryTab extends JPanel {
  
  private val dateFrom = new JXDatePicker(new java.util.Date)
  private val dateTo = new JXDatePicker(new java.util.Date)
  
  private val tzList = new JComboBox(
    (
      List("default","UTC")
      ++ DateTimeZone.getAvailableIDs.toList.filter{_.contains("/")}.sortWith(_<_)
    ).toArray[AnyRef]
  )
  tzList.setSelectedIndex(0)
  tzList.setEditable(false)
  
  private val rootNode = new DefaultMutableTreeNode(ListItem("Categories","all"))
  private val treeModel = new DefaultTreeModel(rootNode);
  private val categories = new JTree(treeModel)
  private val categoryDialog = new NewCategoryDialog(MainWindow)
  treeModel.insertNodeInto(
    new DefaultMutableTreeNode(ListItem("All flows","all")), 
    rootNode,
    0
  )
  def expandCategoryTree(){
    for (i<-0 until categories.getRowCount) {
         categories.expandRow(i)
    }
  }
  expandCategoryTree()
  
  private val bytesStat = new JCheckBox("Bytes")
  private val flowsStat = new JCheckBox("Flows")
  private val packetsStat = new JCheckBox("Packets")
  private val durationStat = new JCheckBox("Duration")
  bytesStat.setSelected(true)
  
  private val sumStatRadio = new JRadioButton("Sum all")
  private val topStatRadio = new JRadioButton("Pick top")
  private val topCountField = new JSpinner(new SpinnerNumberModel(1,1,100,1))
  private val indexListModel = new DefaultListModel
  private val indexList = new JList(indexListModel)
  private val indexDialog = new NewIndexDialog(MainWindow)
  private val addIndexButton = new JButton("Add").withAction{
    indexDialog.get() foreach{ index => 
      indexListModel.addElement(index)
      indexList.ensureIndexIsVisible(indexListModel.getSize - 1)
    }
  }
  private val removeIndexButton = new JButton("Remove").withAction{
    val i = indexList.getSelectedIndex
    if(i>=0){
      indexListModel.remove(i)
    }
  }
  
  private val indexListAndFriends = List(indexList,addIndexButton,removeIndexButton,topCountField)
  sumStatRadio.withAction{
    topStatRadio.setSelected(!sumStatRadio.isSelected)
    indexListAndFriends foreach {_ setEnabled topStatRadio.isSelected}
  }
  topStatRadio.withAction{
    sumStatRadio.setSelected(!topStatRadio.isSelected)
    indexListAndFriends foreach {_ setEnabled topStatRadio.isSelected}
  }
  sumStatRadio.setSelected(true)
  topStatRadio.setSelected(false)
  indexListAndFriends foreach {_ setEnabled false}
  
  
  val periodType = new JComboBox(Array[AnyRef](
    ListItem("All time", "always"),
    ListItem("Each minute", "minute"),
    ListItem("Each hour", "hour"),
    ListItem("Each day",  "day"),
    ListItem("Each week", "week"),
    ListItem("Each month", "month"),
    ListItem("Each year", "year"), 
    ListItem("By hour of day", "hourofday"),
    ListItem("By day of week", "dayofweek"),
    ListItem("By hour of week", "hourofweek")
  ))
  periodType.setSelectedIndex(0)
  
  withLayout(
    this,
    "20dlu,50dlu:g,20dlu,50dlu:g,20dlu",
    "20dlu,f:p:g,20dlu"
  ).add(2,2, "p:g", "p,20dlu,p,20dlu,f:p:g"){
    _.add(1,1,"50dlu:g,5dlu,50dlu:g","p,5dlu,p,5dlu,p"){
      _.add(1,1, 3,1 , "Time range").
      add(1,3,enabler("From",false,dateFrom)).
      add(3,3,enabler("To",false,dateTo)).
      add(1,5,dateFrom).
      add(3,5,dateTo)
    }.add(1,3,"f:0dlu:g,5dlu,f:0dlu:g","p"){
      _.add(1,1, "Timezone").
      add(3,1, tzList)
    }.add(1,5,"30dlu:g,5dlu,30dlu:g,5dlu,30dlu:g","p,5dlu,f:50dlu:g,5dlu,p"){
      _.add(1,1,5,1, "Categories").
      add(1,3,5,1,scrollable(categories)).
      add(1,5,button("Add child"){
        val parentPath = categories.getSelectionPath
        val parentNode = (
          if(parentPath==null) rootNode
          else Option(parentPath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]) getOrElse rootNode
        )
        categoryDialog.get().foreach { item =>
          treeModel.insertNodeInto(
            new DefaultMutableTreeNode(item), 
            parentNode,
            parentNode.getChildCount
          )
          expandCategoryTree()
        }
      }).add(3,5,button("Edit"){
        Option(categories.getSelectionPath) foreach { selection =>
          val currentNode = selection.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          if(currentNode.getParent != null){
            categoryDialog.get().foreach { item =>
              currentNode.setUserObject(item)
              treeModel.nodeChanged(currentNode)
            }
          }
        }
      }).add(5,5,button("Remove"){
        Option(categories.getSelectionPath) foreach { selection =>
          val currentNode = selection.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          if(currentNode.getParent != null){
            treeModel.removeNodeFromParent(currentNode)
          }
        }
      })
    }
  }.add(4,2, "f:0dlu:g", "p,20dlu,p,20dlu,f:p:g,20dlu,p"){
    _.add(1,1,"f:0dlu:g","p,5dlu,p,5dlu,p,5dlu,p,5dlu,p,5dlu,p,5dlu,p"){
      _.add(1,1, "Statistics").
      add(1,3, bytesStat).
      add(1,5, packetsStat).
      add(1,7, flowsStat).
      add(1,9, durationStat)
    }.add(1,3,"f:0dlu:g,5dlu,f:0dlu:g","p"){
      _.add(1,1, "Period").
      add(3,1, periodType)
    }.add(1,5,"p,5dlu,f:30dlu:g,5dlu,f:p","p,5dlu,p,5dlu,p,5dlu,p,0dlu:g"){
      _.add(1,1, sumStatRadio).
      add(1,3, topStatRadio).
      add(3,3, topCountField).
      add(5,3, "elements grouped by:").
      add(3,5, 1,4, scrollable(indexList)).
      add(5,5, addIndexButton).
      add(5,7, removeIndexButton)
    }.add(1,7,"l:p:g","p"){
      _.add(1,1,button("Submit"){
        buildQuery match {
          case Success(q) =>
            MainWindow.agent ! NewQuery(q)
          case Failure(nel) =>
            errorMessage(nel)
        }
      })
    }
  }
  
  def buildTimeWindowPartOfQuery: Validation[NonEmptyList[String],String] = {
    val dateFormatter = new DateTimeFormatterBuilder().
      appendYear(4,4).appendMonthOfYear(2).appendDayOfMonth(2).
      appendHourOfDay(2).appendMinuteOfHour(2).appendSecondOfMinute(2).
      toFormatter
    val timeFrom = new DateTime(dateFrom.getDate).toString(dateFormatter)
    val timeTo = new DateTime(dateTo.getDate).plus(Period.days(1)).toString(dateFormatter)
    (dateFrom.isEnabled, dateTo.isEnabled) match{
      case (false, false) => "all".success
      case (true, false) => ("from "+timeFrom).success
      case (false, true) => ("to "+timeTo).success
      case (true,true) =>
        if(timeFrom>=timeTo) "Invalid time range".failNel 
        else (timeFrom + " - " + timeTo).success
    }
  }
  
  def buildTzPartOfQuery: Validation[NonEmptyList[String],String] = (
    "tz "+tzList.getSelectedItem
  ).success
  
  def buildSubnetsPartOfQuery: Validation[NonEmptyList[String],String] = SubnetCache.code.success

  def categoryNodeToString(node: DefaultMutableTreeNode): String = {
    val header = node.getUserObject.asInstanceOf[ListItem].code
    if(node.getChildCount == 0) header
    else {
      header + (0 until node.getChildCount).map { i =>
        categoryNodeToString(node.getChildAt(i).asInstanceOf[DefaultMutableTreeNode])
      }.mkString(" [", ", ", "]")
    }
  }
  def buildSplitFilterPartOfQuery: Validation[NonEmptyList[String],String] = {
    if(rootNode.getChildCount == 0) return "No categories".failNel
    categoryNodeToString(rootNode).success
  }
  
  def buildSummableFieldsPartOfQuery: Validation[NonEmptyList[String],String] = {
    var fields = List[String]()
    if (durationStat.isSelected) fields = "duration"::fields
    if (flowsStat.isSelected) fields = "flows"::fields
    if (packetsStat.isSelected) fields = "packets"::fields
    if (bytesStat.isSelected) fields = "bytes"::fields
    if (fields.isEmpty) "No statistic selected".failNel
    else fields.mkString(", ").success
  }

  def getIndexableFields : Validation[NonEmptyList[String],List[String]] = {
    if(sumStatRadio.isSelected) Nil.success
    else {
      val l = (0 until indexListModel.getSize).map {
        indexListModel.get(_).asInstanceOf[ListItem].code
      }.toList 
      l match {
        case Nil => "No index fields chosen".failNel
        case i => i.success
      }
    }
  }
  
  def buildTopPartOfQuery: Validation[NonEmptyList[String],String] = {
    getIndexableFields map {
      case Nil => ""
      case indices => "top "+topCountField.getValue+" by "+indices.mkString(", ")
    }
  }
  
  def buildPeriodPartOfQuery: Validation[NonEmptyList[String],String] = {
    try{
      periodType.getSelectedItem.asInstanceOf[ListItem].code.success 
    } catch {
      case _ => "Invalid period selected".failNel
    }
  }
  
  def buildQuery: Validation[NonEmptyList[String],String] = {
    (
        buildTimeWindowPartOfQuery |@| 
        buildTzPartOfQuery |@| 
        buildSubnetsPartOfQuery |@|
        buildSplitFilterPartOfQuery |@|
        buildSummableFieldsPartOfQuery |@|
        buildTopPartOfQuery |@|
        buildPeriodPartOfQuery
    ) apply (_+" "+_+";"+_+";"+_+";"+_+" "+_+" each "+_)
  }
}