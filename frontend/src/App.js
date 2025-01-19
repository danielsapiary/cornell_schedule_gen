import React, { useState, useEffect, useCallback } from 'react';
import { Calendar, momentLocalizer } from 'react-big-calendar';
import moment from 'moment';
import 'react-big-calendar/lib/css/react-big-calendar.css';
const localizer = momentLocalizer(moment);

const generateRandomColor = () => {
  const colors = [
    '#FF5733', '#33FF57', '#3357FF', '#FFC300', '#C70039', '#900C3F', '#581845',
    '#DAF7A6', '#FF33FF', '#33FFFF', '#FFA07A', '#20B2AA', '#9370DB', '#3CB371',
    '#7B68EE', '#6A5ACD', '#FF69B4', '#FF6347', '#4682B4', '#D2B48C', '#6495ED',
    '#40E0D0', '#8A2BE2', '#FF4500', '#2E8B57', '#F08080', '#FF1493', '#FFD700',
    '#ADFF2F', '#87CEEB', '#FFB6C1', '#32CD32', '#7FFFD4', '#8B0000', '#BA55D3'
  ];

  return colors[Math.floor(Math.random() * colors.length)];
};

const courseColors = {}; // Store colors for each course title

// Event style getter to apply the color
const eventStyleGetter = (event) => {
  if (event.type === 'selectedInterval') {
    const style = {
      backgroundColor: 'rgba(200, 0, 0, 0.5)',
      borderRadius: '0px',
      opacity: 0.8,
      color: 'white',
      border: 'none',
      display: 'block',
    };
    return { style };
  } else {
    const courseTitle = event.title.split(' ')[0];
    if (!courseColors[courseTitle]) {
      courseColors[courseTitle] = generateRandomColor();
    }
    const backgroundColor = courseColors[courseTitle];
    const style = {
      backgroundColor: backgroundColor,
      borderRadius: '4px',
      opacity: 0.9,
      color: 'white',
      border: 'none',
      display: 'block',
    };
    return { style };
  }
};

function App() {
  const [schedulesLoaded, setSchedulesLoaded] = useState(false);
  const [input, setInput] = useState('');
  const [messages, setMessages] = useState([]);
  const [scheduleIndex, setScheduleIndex] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const [selectedIntervals, setSelectedIntervals] = useState([]);
  const [selectedIntervalsEvents, setSelectedIntervalsEvents] = useState([]);

  const mondayMidnight = new Date(1970, 0, 5, 0, 0, 0);

  const handleSubmit = useCallback(() => {
    if (input.trim()) {
      // clearCalendar();
      setLoading(true);
      setError(null);
      setMessages([]);
  
      // Adjust intervals to ensure no negative values before sending the POST request
      const adjustedIntervals = selectedIntervals.map((interval) => {
        if (interval.start < 0) {
          return {
            start: interval.start + 10080,
            end: interval.end + 10080,
          };
        }
        return interval;
      });
  
      const intervalsString =
        adjustedIntervals.length > 0
          ? formatIntervals(adjustedIntervals)
          : 'default'; // Default to full week if no intervals selected
  
      const requestBody = intervalsString + ' ' + input.trim();
  
      fetch(`http://${window.location.hostname}:9000/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'text/plain',
        },
        body: requestBody,
      })
        .then((response) => response.json())
        .then((data) => {
          setLoading(false);
          if (data.error) {
            setError(data.error);
            setMessages([]);
          } else {
            setMessages(data);
            setSchedulesLoaded(true);
            setScheduleIndex(0);
            setError(null);
          }
        })
        .catch((err) => {
          setLoading(false);
          if (err.message.includes('Failed to fetch')) {
            setError('Server is down. Please try again later.');
          } else {
            setError('Failed to fetch schedules from server.');
          }
        });
    }
  }, [input, selectedIntervals]);

  const prevSchedule = useCallback(() => {
    if (scheduleIndex > 0) setScheduleIndex(scheduleIndex - 1);
  }, [scheduleIndex]);

  const nextSchedule = useCallback(() => {
    if (scheduleIndex < messages.length - 1) setScheduleIndex(scheduleIndex + 1);
  }, [scheduleIndex, messages.length]);

  useEffect(() => {
    const handleKeyDown = (event) => {
      if (event.key === 'Enter') {
        handleSubmit();
      } else if (event.key === 'ArrowLeft') {
        prevSchedule();
      } else if (event.key === 'ArrowRight') {
        nextSchedule();
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleSubmit, prevSchedule, nextSchedule]);

  const handleSelectSlot = (slotInfo) => {
    if (schedulesLoaded) return;
  
    let dayAdjustment = slotInfo.start.getDay() - 1; // Sunday is day 0, Monday is day 1, etc.
  
    // Calculate start and end times in minutes since Monday midnight
    const startMinutes = dayAdjustment * 1440 + slotInfo.start.getHours() * 60 + slotInfo.start.getMinutes();
    const endMinutes = dayAdjustment * 1440 + slotInfo.end.getHours() * 60 + slotInfo.end.getMinutes();
  
    // Ensure the selected time range is within bounds (-1440 to 10079 minutes)
    if (startMinutes >= -1440 && endMinutes <= 10079) {
      const newInterval = { start: startMinutes, end: endMinutes };
      const mergedIntervals = mergeIntervals([...selectedIntervals, newInterval]);
      setSelectedIntervals(mergedIntervals);
  
      // Create new events from merged intervals using mondayMidnight as the reference
      const newEvents = mergedIntervals.map((interval) => ({
        title: 'Busy',
        start: new Date(mondayMidnight.getTime() + interval.start * 60000),
        end: new Date(mondayMidnight.getTime() + interval.end * 60000),
        type: 'selectedInterval',
      }));
  
      setSelectedIntervalsEvents(newEvents);
    } else {
      console.warn('Selected time is out of bounds.');
    }
  };

  const handleEventClick = (event) => {
    if (event.type === 'selectedInterval' && !schedulesLoaded) {
      const updatedIntervals = selectedIntervals.filter(
        (interval) =>
          !(interval.start === (event.start - mondayMidnight.getTime()) / 60000 &&
            interval.end === (event.end - mondayMidnight.getTime()) / 60000)
      );
  
      setSelectedIntervals(updatedIntervals);
  
      const newEvents = updatedIntervals.map((interval) => ({
        title: 'Busy',
        start: new Date(mondayMidnight.getTime() + interval.start * 60000),
        end: new Date(mondayMidnight.getTime() + interval.end * 60000),
        type: 'selectedInterval',
      }));
      setSelectedIntervalsEvents(newEvents);
    }
  };
  

  const mergeIntervals = (intervals) => {
    if (intervals.length === 0) return [];
    intervals.sort((a, b) => a.start - b.start);
    const merged = [intervals[0]];
    for (let i = 1; i < intervals.length; i++) {
      const last = merged[merged.length - 1];
      if (intervals[i].start <= last.end) {
        last.end = Math.max(last.end, intervals[i].end);
      } else {
        merged.push(intervals[i]);
      }
    }
    return merged;
  };

  const formatIntervals = (intervals) => {
    return intervals.map((interval) => `${interval.start},${interval.end}`).join(';');
  };

  const clearCalendar = () => {
    setSchedulesLoaded(false);
    setSelectedIntervals([]);
    setSelectedIntervalsEvents([]);
    setMessages([]);
    setScheduleIndex(0);
  };

  const convertToEvents = (schedule) => {
    return schedule
      .map((course) => {
        const courseEvents = [];
        ['lecture', 'discussion', 'lab'].forEach((sessionType) => {
          if (course[sessionType]) {
            course[sessionType].forEach((session) => {
              const sessionNameField = `${sessionType}_name`; // e.g., 'lecture_name', 'discussion_name', 'lab_name'
              const sessionName = course[sessionNameField] || ''; // Get the name or fallback to an empty string
              const start = new Date(mondayMidnight.getTime() + session.start * 60000);
              const end = new Date(mondayMidnight.getTime() + session.end * 60000);
              courseEvents.push({
                title: `${course.title} ${sessionName} `,
                start,
                end,
                color: generateRandomColor(),
              });
            });
          }
        });
        return courseEvents;
      })
      .flat();
  };  

  const classEvents =
    messages.length > 0 ? convertToEvents(messages[scheduleIndex]) : [];
  const calendarEvents = [...selectedIntervalsEvents, ...classEvents];

  return (
    <div style={styles.container}>
      <style>
        {`
          .rbc-allday-cell, .rbc-row-bg .rbc-allday-cell, .rbc-time-header-cell.rbc-header {
            display: none;
          }
          
          .rbc-event {
            font-size: 14px; /* Adjust to your desired size */
            padding: 2px 4px; /* Optional: Adjust padding for better fit */
          }
        `}
      </style>
      <h1>Schedule Generator</h1>
      <p>Enter course names to generate possible schedules:</p>
      <input
        type="text"
        value={input}
        placeholder="e.g., CS2110 MATH1920 ECON1110 (Courses seperated by spaces)"
        onChange={(e) => setInput(e.target.value)}
        style={styles.input}
      />
      <button onClick={handleSubmit} style={styles.button}>
        Generate Schedules
      </button>
      <button onClick={clearCalendar} style={styles.button}>
        Clear Calendar
      </button>
      {loading && <p style={styles.loading}>🐪</p>}
      {error && (
        <div style={styles.errorContainer}>
          <p style={styles.errorMessage}>Error: {error}</p>
        </div>
      )}
      <div style={styles.calendarContainer}>
        {messages.length > 0 && !loading && (
          <h2>
            Schedule {scheduleIndex + 1} of {messages.length}
          </h2>
        )}
        <Calendar
          localizer={localizer}
          events={calendarEvents}
          startAccessor="start"
          endAccessor="end"
          style={{ height: 600, width: '100%' }}
          defaultView="week"
          views={['week']}
          defaultDate={new Date(1970, 0, 5)}
          min={new Date(1970, 0, 5, 8, 0, 0)}
          max={new Date(1970, 0, 5, 22, 0, 0)}
          onNavigate={(date, view, action) => {
            if (action === 'NEXT' || action === 'PREV' || action === 'DATE') {
              return;
            }
          }}
          toolbar={false}
          eventPropGetter={eventStyleGetter}
          formats={{
            dayFormat: (date, culture, localizer) =>
              localizer.format(date, 'dddd', culture),
          }}
          selectable={!schedulesLoaded}
          onSelectSlot={handleSelectSlot}
          onSelectEvent={handleEventClick}
          dragFromOutsideItem={null}
          resizable
          
        />
        {messages.length > 0 && !loading && (
          <div style={styles.navigation}>
            <button onClick={prevSchedule} disabled={scheduleIndex === 0}>
              Previous
            </button>
            <button
              onClick={nextSchedule}
              disabled={scheduleIndex === messages.length - 1}
            >
              Next
            </button>
          </div>
        )}
      </div>
      {messages.length === 0 && !loading && !error && (
        <p style={{ marginTop: '20px', color: 'gray', fontSize: '16px' }}>
          Click and drag to indicate busy times. Click to remove busy times. Then, enter valid course names and click Generate Schedules.
        </p>
      )}
      {schedulesLoaded && (
        <p style={{ marginTop: '20px', color: 'gray', fontSize: '16px' }}>
          Please clear the calendar to select new busy times.
        </p>
      )}
    </div>
    
  );
}

const styles = {
  container: {
    textAlign: 'center',
    fontFamily: 'Arial, sans-serif',
    padding: '20px',
    maxWidth: '1200px',
    margin: 'auto',
  },
  input: {
    width: '60%',
    padding: '8px',
    margin: '10px 0',
    fontSize: '16px',
  },
  button: {
    padding: '10px 20px',
    fontSize: '16px',
    cursor: 'pointer',
  },
  loading: {
    fontSize: '40px',
    marginTop: '20px',
    animation: 'spin 1s linear infinite',
  },
  errorContainer: {
    backgroundColor: '#ffdddd',
    padding: '10px',
    borderRadius: '5px',
    marginTop: '20px',
  },
  errorMessage: {
    color: '#d9534f',
    fontSize: '16px',
    fontWeight: 'bold',
  },
  calendarContainer: {
    marginTop: '20px',
  },
  navigation: {
    display: 'flex',
    justifyContent: 'space-between',
    marginTop: '10px',
  },
};

const styleSheet = document.styleSheets[0];
styleSheet.insertRule(
  `
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
`,
  styleSheet.cssRules.length
);

export default App;