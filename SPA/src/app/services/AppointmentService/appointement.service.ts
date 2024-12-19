import {Injectable} from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class AppointementService {
  private apiUrl = json.apiUrl + '/appointment';


  constructor(private http: HttpClient) {
  }

  createAppointment(appointmentDTO: any): Observable<string> {
    const appointment = {
      id: null,
      operationRequestId: appointmentDTO.operationRequestID.value,
      surgeryRoomId: appointmentDTO.surgeryRoom,
      surgeryDate: appointmentDTO.surgeryDate,
      status:null
    };
    console.log('Creating appointment:', appointment);
    return this.http.post<string>(`${this.apiUrl}`, appointment, {withCredentials: true});
  }
}
