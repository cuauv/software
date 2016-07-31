import os, re, confParser
from serial.tools import list_ports
try:
    import wx
    import wx.grid
except ImportError:
    raise ImportError,"The wxPython module is required to run this program."
class mainwindow(wx.Frame):

	def __init__(self,parent,id,title):
		wx.Frame.__init__(self,parent,id,title)
		self.parent = parent
		self.menuBar = wx.MenuBar()
		self.fileMenu = wx.Menu()
		exitMenuItem  = self.fileMenu.Append(wx.NewId(), "Exit", "Exit the application")
		#self.Bind(wx.EVT_MENU, self.Close, exitMenuItem)
		self.menuBar.Append(self.fileMenu,"&File")
		self.SetMenuBar(self.menuBar)
		self._board = None
		self.baud_rates = [9600, 19200, 57600, 203400]

		self.sizer = wx.GridBagSizer()
		self.LoadConfigButton = wx.Button(self, id=-1, label="Load Config")
		self.LoadConfigButton.Bind(wx.EVT_BUTTON, self.loadConfig)
		self.sizer.Add(self.LoadConfigButton,(0,0),(1,1))
		#serial_list = list_serial_ports()
		self.combo = wx.ComboBox(self, choices=["her","der"])
		self.sizer.Add(self.combo,(1,4),(1,5),wx.EXPAND)
		
		print serial_list
		self.SetSizerAndFit(self.sizer)
		self.Show(True)
	def generateRegisterPanel(self, brd):
		panel = wx.Panel(self,wx.ID_ANY)
		self.register_grid = wx.grid.Grid(panel)
		self.register_grid.CreateGrid(len(self._board._registers),5)
		self.populate_registerGrid(self.register_grid,brd._registers)
		sizer = wx.BoxSizer(wx.VERTICAL)
		panel.SetMaxSize((1000,400))
		sizer.Add(self.register_grid, 1, wx.EXPAND, 5)
		panel.SetSizer(sizer)
		return panel
	def setBoard(self, brd):
		self._board = brd
		self.sizer.Add(self.generateRegisterPanel(self._board), (4,0), (6,8),wx.EXPAND)
		self.SetSizerAndFit(self.sizer)
	def populate_registerGrid(self, grid, registers):
		column_labels = ["name", "type", "isPointer", "read", "write"]
		for i, label in enumerate(column_labels):
			grid.SetColLabelValue(i,label)
		for i, reg in enumerate(registers.iteritems()):
			print reg
			grid.SetCellValue(i,0,reg[1].name)
	def loadConfig(self, event=None):
		fdlg = wx.FileDialog(self, "Open a conf File", wildcard="*.conf")
		result = fdlg.ShowModal()
		path = None
		if result == wx.ID_OK:
			path = fdlg.GetPath()
		del fdlg
		self.setBoard(confParser.parseConfigurationFile(path))
		return path
def list_serial_ports():
    # Windows
    if os.name == 'nt':
        # Scan for available ports.
        available = []
        for i in range(256):
            try:
                s = serial.Serial(i)
                available.append('COM'+str(i + 1))
                s.close()
            except serial.SerialException:
                pass
        return available
    else:
        # Mac / Linux
        #return [port[0] for port in list_ports.comports()]
        return [port[0] for port in list_ports.comports() if re.match(".*(USB|ACM).*",port[0])]
if __name__ == "__main__":
    app = wx.App()
    frame = mainwindow(None,-1,'Python Serial Debugger')
    app.MainLoop()

