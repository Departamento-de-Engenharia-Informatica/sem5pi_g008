import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {catchError, Observable, of} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class SurgeryRoomService {

  private apiUrl = json.apiUrl + '/surgeryRoom';
  private SwiUrl = "http://localhost:8080";
  private algavUrl = json.apiUrl + '/algav';



  constructor(private http: HttpClient) {
  }

  getSurgeryRooms(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/status`, {
      withCredentials: true
    }).pipe(
      catchError(error => {
        console.error('Error getting surgery rooms:', error);
        return of([]);
      })
    );
  }

  getSurgeryPlan(date: string, room: string): Observable<any[]> {
    const params = new URLSearchParams();
    params.set('date', date);
    params.set('room', room);

    return this.http.get<any[]>(`${this.SwiUrl}/generate_plan?${params.toString()}`).pipe(
      catchError(error => {
        console.error('Error getting surgery plan:', error);
        return of([]);
      })
    );
  }


  loadData(): Observable<any[]> {
    return this.http.get<any[]>(`${this.SwiUrl}/load`).pipe(
      catchError(error => {
        console.error('Error getting surgery plan:', error);
        return of([]);
      })
    );
  }


  getOperationTypes(): Observable<any> {
    return this.http.get(`${this.algavUrl}/operation-type`);
  }
  getRequests(): Observable<any> {
    return this.http.get(`${this.algavUrl}/operation-request`);
  }

   getSurgeryRoomsInfo(): Observable<any> {
    return this.http.get(`${this.algavUrl}/surgery-room`);
  }

  getStaff(): Observable<any> {
    return this.http.get(`${this.algavUrl}/staff`);
  }
  getRequiredStaff(): Observable<any> {
    return this.http.get(`${this.algavUrl}/RequiredStaff`);
  }
  getData(): Observable<any> {
    return this.http.get(`${this.SwiUrl}/getdata`);
  }

  postSurgeryPlanRoom(agenda: any): Observable<any> {
    const payload = {
      date: agenda[0].date,
      room_id: agenda[0].room_id,
      task: agenda[0].tasks
    };
    console.log('Payload being sent:', payload);
    console.log('service----Room agenda-- service:', agenda);
    return this.http.patch(`${this.algavUrl}/updateRoomAgenda`, agenda, { withCredentials: true });
  }
  postSurgeryPlanStaff(agenda: any): Observable<any> {
    const payload = {
      agenda: agenda
    };
    console.log('Payload being sent:', payload);

    return this.http.patch(`${this.algavUrl}/updateStaffAgenda`, payload, { withCredentials: true });
  }

}
