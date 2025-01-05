import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {catchError, forkJoin, Observable, of, tap} from 'rxjs';
import json from '../../appsettings.json';

@Injectable({
  providedIn: 'root'
})
export class SurgeryRoomService {

  private apiUrl = json.backendApi["1"].url + '/surgeryRoom';
  private SwiUrl = "http://localhost:8080";
  private algavUrl = json.backendApi["1"].url + '/algav';

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
  // getSurgeryPlan(): Observable<any[]> {
  //   return this.http.get<any[]>(`${this.SwiUrl}/generate_plan`).pipe(
  //     catchError(error => {
  //       console.error('Error getting surgery plan:', error);
  //       return of([]);
  //     })
  //   );
  // }


  loadData(): Observable<any[]> {
    return this.http.get<any[]>(`${this.SwiUrl}/load`).pipe(
      catchError(error => {
        console.error('Error getting surgery plan:', error);
        return of([]);
      })
    );
  }
  getData(): Observable<any> {
    return this.http.get(`${this.SwiUrl}/getdata`);
  }


  postSurgeryPlanRoom(agenda: any): Observable<any> {
    const requests: Observable<any>[] = [];

    for (let i = 0; i < agenda.length; i++) {
      const payload = {
        date: agenda[i].date,
        room_id: agenda[i].room_id,
        task: agenda[i].tasks
      };

      console.log('Payload being sent:', payload);
      console.log('service----Room agenda-- service:', agenda[i]);

      const request = this.http.patch(`${this.algavUrl}/updateRoomAgenda`, payload, { withCredentials: true });
      requests.push(request);
    }

    // Combina todas as requisições em sequência e aguarda a conclusão de todas
    return forkJoin(requests).pipe(
      tap(() => console.log('All requests have been processed successfully.'))
    );
  }

  postSurgeryPlanStaff(agenda: any): Observable<any> {
    const requests: Observable<any>[] = [];

    for (let i = 0; i < agenda.length; i++) {
      const payload = {
        date: agenda[i].date,
        room_id: agenda[i].staff_id,
        task: agenda[i].tasks
      };

      console.log('Payload being sent:', payload);
      console.log('service----Room agenda-- service:', agenda[i]);

      const request = this.http.patch(`${this.algavUrl}/updateStaffAgenda`, payload, { withCredentials: true });
      requests.push(request);
    }

    // Combina todas as requisições em sequência e aguarda a conclusão de todas
    return forkJoin(requests).pipe(
      tap(() => console.log('All requests have been processed successfully.'))
    );
  }



  getRoomInfo(): Observable<{ title: string, body: string | number }[][]> {
    return this.http.get<{ title: string, body: string | number }[][]>(`${this.apiUrl}/info`, {
      withCredentials: true
    }).pipe(
      catchError(error => {
        console.error('Error getting room info:', error);
        return of([]);
      })
    );
  }
}
