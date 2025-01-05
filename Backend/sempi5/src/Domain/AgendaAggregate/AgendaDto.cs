public class AgendaDto
{
    public int date { get; set; }
    public string room_id { get; set; }
    public List<tasks> task { get; set; }


    public class tasks
    {
        public string code { get; set; }
        public int end { get; set; }
        public int start { get; set; }
    }
}