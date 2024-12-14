import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import json from '../../appsettings.json';
import {SpecializationDTO} from '../../DTO/SpecializationDTO';

@Injectable({
  providedIn: 'root'
})
export class SpecializationService {
  private apiUrl = json.apiUrl + '/Specialization';

  constructor(private http: HttpClient) {}

  public listAllSpecializations(): Observable<SpecializationDTO[]> {

    return this.http.get<SpecializationDTO[]>(`${this.apiUrl}`, { withCredentials: true });
  }
}
