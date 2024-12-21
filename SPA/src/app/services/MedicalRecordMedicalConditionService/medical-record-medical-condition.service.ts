import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {Allergy} from '../../Domain/Allergy';
import {catchError, Observable, throwError} from 'rxjs';
import {MedicalRecordCondition} from '../../Domain/MedicalRecordCondition';

@Injectable({
  providedIn: 'root'
})
export class MedicalRecordMedicalConditionService {

  private apiUrl = json.backendApi["2"].url + '/medicalRecordCondition';

  constructor(private http: HttpClient) {
  }

  listMedicalRecordConditionsByMedicalRecordId(medicalRecordId: string) : Observable<{ medicalRecordConditions: MedicalRecordCondition[] }>{
    const url = `${this.apiUrl}?recordNumberId=${medicalRecordId}`;

    return this.http.get<{ medicalRecordConditions: MedicalRecordCondition[] }>(url, {
      withCredentials: true
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status === 850) {
          errorMessage = error.error.message;
        }

        return throwError(errorMessage);
      })
    );
  }


}
