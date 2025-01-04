
import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {OperationRequest} from '../../Domain/OperationRequest';
import {Observable, of} from 'rxjs';
import {catchError} from 'rxjs/operators';
import json from '../../appsettings.json';
import {PatientProfile} from '../../Domain/PatientProfile';


@Injectable({
  providedIn: 'root'
})
export class OperationRequestService {

  private apiUrl1 = json.backendApi["1"].url + '/staff';
  private apiUrl = json.backendApi["1"].url + '/operationRequest';

  constructor(private http: HttpClient) {}

  searchRequests(filters: any): Observable<any> {
    const { patientName, operationType, priority, status } = filters;
    const normalizedFilters = {
      patientName: patientName === '' ? null : patientName,
      operationType: operationType === '' ? null : operationType,
      priority: priority === '' ? null : priority,
      status: status === '' ? null : status
    };
      return this.http.get<any>(`${this.apiUrl1}/search/requests/${normalizedFilters.patientName}/${normalizedFilters.operationType}/${normalizedFilters.priority}/${normalizedFilters.status}/`, {
      withCredentials: true
    });
  }

  deleteOperationRequest(operationId: string): Observable<any> {
    return this.http.delete(`${this.apiUrl1}/request/deleteRequest/${operationId}`, {
      withCredentials: true
      }
    );
  }

  addOperationRequest(operation: OperationRequest): Observable<string> {
    return this.http.post<string>(`${this.apiUrl}/requestOperation`, operation, {
      withCredentials: true,
    }).pipe(
      catchError(error => {
        console.error('Error adding operation request:', error);
        return of('');
      })
    );
  }

  public editOperationRequestDeadline(operation: OperationRequest, deadline:string, operationRequestId: string): Observable<any> {

    return this.http.put(`${this.apiUrl}/OperationRequest/updateOperationRequestDeadline/${operationRequestId}/${deadline}}`, operation, { withCredentials: true });
  }

  public editOperationRequestPriority(operation: OperationRequest, priority:string, operationRequestId: string): Observable<any> {

    return this.http.put(`${this.apiUrl}/OperationRequest/updateOperationRequestDeadline/${operationRequestId}/${priority}}`, operation, { withCredentials: true });
  }

  public listAllPatientProfilesNames(): Observable<PatientProfile[]> {

    return this.http.get<PatientProfile[]>(`${this.apiUrl}/Patient`, { withCredentials: true });
  }

  public listDoctorsOperationRequests(): Observable<OperationRequest[]> {

    return this.http.get<OperationRequest[]>(`${this.apiUrl}/OperationRequest`, { withCredentials: true });
  }

}
