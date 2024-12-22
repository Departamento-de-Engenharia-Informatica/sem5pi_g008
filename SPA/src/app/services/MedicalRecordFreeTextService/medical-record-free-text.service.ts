import {Injectable} from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, Observable, throwError} from 'rxjs';
import {MedicalRecordCondition} from '../../Domain/MedicalRecordCondition';
import {MedicalRecordFreeText} from '../../Domain/MedicalRecordFreeText';


@Injectable({
  providedIn:'root'
})

export class MedicalRecordFreeTextService{
  private apiUrl=json.backendApi["2"].url + '/medicalRecordFreeText';

  constructor(private http: HttpClient) {
  }


  listMedicalRecordFreeTextByMedicalRecordId(medicalRecordId: string) : Observable<{ medicalRecordFreeText: MedicalRecordFreeText[] }>{
    const url = `${this.apiUrl}?recordNumberId=${medicalRecordId}`;

    return this.http.get<{ medicalRecordFreeText: MedicalRecordFreeText[] }>(url, {
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
