import {Injectable} from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {AppointmentDTO} from '../../DTOs/AppointmentDto/AppointmentDto';

@Injectable({
  providedIn: 'root'
})
export class AppointementService {
  private apiUrl =  json.backendApi["1"].url + '/appointment';


  constructor(private http: HttpClient) {
  }

  createAppointment(appointmentDTO: any): Observable<string> {
    const appointment = {
      id: null,
      operationRequestId: appointmentDTO.operationRequestID,//TOOD: check if this is correct with value or without value
      surgeryRoomId: appointmentDTO.surgeryRoom,
      surgeryDate: appointmentDTO.surgeryDate,
      status:null
    };
    console.log('Creating appointment:', appointment);
    return this.http.post<string>(`${this.apiUrl}`, appointment, {withCredentials: true});
  }
  updateAppointment(apointmentDTO: AppointmentDTO) {
    console.log('Updating appointment:', apointmentDTO);
    return this.http.patch(`${this.apiUrl}`, apointmentDTO, {withCredentials: true});
  }

  getAppointments() {
    return this.http.get<AppointmentDTO[]>(`${this.apiUrl}`, {withCredentials: true});
  }
}
